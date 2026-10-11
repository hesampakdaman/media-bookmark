;;; media-bookmark-mpv.el --- Media-bookmark backend for mpv -*- lexical-binding: t; -*-

;; Author: Hesam Pakdaman <https://github.com/hesampakdaman>
;; Maintainer: Hesam Pakdaman <https://github.com/hesampakdaman>
;; URL: https://github.com/hesampakdaman/media-bookmark
;; Assisted-by: Claude:claude-opus-5-5

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The `mpv' backend of media-bookmark.  It starts an mpv process per
;; session and controls it through mpv's JSON IPC socket (see "JSON
;; IPC" in the mpv manual).  mpv's position, duration, pause state and
;; title are observed as they change and reported to the session.
;;
;; This file only implements the backend interface of
;; media-bookmark.el, and can serve as a template for other players.

;;; Code:

(require 'cl-lib)
(require 'media-bookmark)
(require 'subr-x)

(defgroup media-bookmark-mpv nil
  "The mpv backend of media-bookmark."
  :group 'media-bookmark
  :prefix "media-bookmark-mpv-")

(defcustom media-bookmark-mpv-program "mpv"
  "The mpv executable."
  :type 'string)

(defcustom media-bookmark-mpv-arguments nil
  "Extra command-line arguments for mpv."
  :type '(repeat string))

(cl-defstruct (media-bookmark-mpv--state
               (:constructor media-bookmark-mpv--make-state)
               (:copier nil))
  "State of an mpv process and its IPC connection."
  process connection socket (pending "") finished)

(defun media-bookmark-mpv--send (mpv &rest command)
  "Send COMMAND, a list of strings and numbers, to MPV."
  (when-let* ((connection (media-bookmark-mpv--state-connection mpv))
              ((process-live-p connection)))
    (process-send-string
     connection
     (concat (json-serialize (list :command (vconcat command))) "\n"))))

(cl-defmethod media-bookmark-backend-play ((_backend (eql 'mpv)) session start)
  "Play SESSION's file in mpv from START, or where mpv chooses if nil."
  (let* ((file (media-bookmark-session-file session))
         (socket (expand-file-name
                  (make-temp-name "media-bookmark-mpv-")
                  (or (getenv "XDG_RUNTIME_DIR") temporary-file-directory)))
         (mpv (media-bookmark-mpv--make-state :socket socket)))
    (setf (media-bookmark-session-data session) mpv)
    (setf (media-bookmark-mpv--state-process mpv)
          (make-process
           :name "media-bookmark-mpv"
           :buffer (generate-new-buffer " *media-bookmark-mpv*")
           :noquery t
           :connection-type 'pipe
           :command `(,media-bookmark-mpv-program
                      ,(concat "--input-ipc-server=" socket)
                      ;; Our position, not mpv's own resume position.
                      ,@(when start
                          (list (format "--start=%s" start)
                                "--resume-playback=no"))
                      ,@media-bookmark-mpv-arguments
                      "--"
                      ,(if (media-bookmark-url-p file)
                           file
                         (expand-file-name file)))
           :sentinel (lambda (process _event)
                       (media-bookmark-mpv--sentinel session mpv process))))
    (media-bookmark-mpv--connect session mpv 100)))

(defun media-bookmark-mpv--connect (session mpv tries)
  "Connect to the IPC socket of MPV, playing SESSION.
mpv creates the socket shortly after it starts, so retry TRIES times."
  (when (and (eq mpv (media-bookmark-session-data session))
             (process-live-p (media-bookmark-mpv--state-process mpv)))
    (condition-case nil
        (progn
          (setf (media-bookmark-mpv--state-connection mpv)
                (make-network-process
                 :name "media-bookmark-mpv-ipc"
                 :family 'local
                 :service (media-bookmark-mpv--state-socket mpv)
                 :coding 'utf-8
                 :noquery t
                 :sentinel #'ignore
                 :filter (lambda (_process string)
                           (media-bookmark-mpv--filter session mpv string))))
          ;; The quit command closes the socket before we could ask
          ;; for the position, so follow the position as it changes.
          (cl-loop for property in '("time-pos" "duration" "pause" "media-title")
                   for id from 1
                   do (media-bookmark-mpv--send mpv "observe_property" id property)))
      (file-error
       (if (> tries 0)
           (run-at-time 0.05 nil #'media-bookmark-mpv--connect
                        session mpv (1- tries))
         (message "media-bookmark: Cannot connect to mpv at %s"
                  (media-bookmark-mpv--state-socket mpv)))))))

(defun media-bookmark-mpv--filter (session mpv string)
  "Handle STRING, output of the IPC connection of MPV, playing SESSION."
  (let ((lines (split-string (concat (media-bookmark-mpv--state-pending mpv) string)
                             "\n")))
    (setf (media-bookmark-mpv--state-pending mpv) (car (last lines)))
    (dolist (line (butlast lines))
      (when-let* ((message (ignore-errors
                             (json-parse-string line
                                                :object-type 'plist
                                                :null-object nil
                                                :false-object nil))))
        (media-bookmark-mpv--handle session mpv message)))))

(defun media-bookmark-mpv--handle (session mpv message)
  "Handle MESSAGE, a parsed IPC message of MPV, playing SESSION."
  (pcase (plist-get message :event)
    ("property-change"
     (let ((data (plist-get message :data)))
       (pcase (plist-get message :name)
         ("time-pos" (when data (media-bookmark-update session :time data)))
         ("duration" (when data (media-bookmark-update session :duration data)))
         ("pause" (media-bookmark-update session :paused data))
         ("media-title" (when data (media-bookmark-update session :title data))))))
    ("end-file"
     (when (equal (plist-get message :reason) "eof")
       (setf (media-bookmark-mpv--state-finished mpv) t)))))

(defun media-bookmark-mpv--sentinel (session mpv process)
  "Clean up after PROCESS, the mpv of MPV, exits, and stop SESSION."
  (unless (process-live-p process)
    (when-let* ((connection (media-bookmark-mpv--state-connection mpv)))
      (delete-process connection))
    (ignore-errors (delete-file (media-bookmark-mpv--state-socket mpv)))
    (let ((output (process-buffer process))
          (status (process-exit-status process)))
      ;; mpv exits with 4 when killed by a signal, as when we stop it.
      (unless (memq status '(0 4))
        (message "mpv exited with status %d: %s" status
                 (with-current-buffer output
                   (string-trim (buffer-string)))))
      (kill-buffer output))
    (when (eq mpv (media-bookmark-session-data session))
      (media-bookmark-stopped session (media-bookmark-mpv--state-finished mpv)))))

(cl-defmethod media-bookmark-backend-seek ((_backend (eql 'mpv)) session time)
  "Seek mpv playing SESSION to TIME."
  (media-bookmark-mpv--send (media-bookmark-session-data session)
                            "seek" time "absolute"))

(cl-defmethod media-bookmark-backend-toggle-pause ((_backend (eql 'mpv)) session)
  "Pause or resume mpv playing SESSION."
  (media-bookmark-mpv--send (media-bookmark-session-data session)
                            "cycle" "pause"))

(cl-defmethod media-bookmark-backend-stop ((_backend (eql 'mpv)) session)
  "Quit mpv playing SESSION."
  (let ((mpv (media-bookmark-session-data session)))
    (if (process-live-p (media-bookmark-mpv--state-connection mpv))
        (media-bookmark-mpv--send mpv "quit")
      (delete-process (media-bookmark-mpv--state-process mpv)))))

(provide 'media-bookmark-mpv)
;;; media-bookmark-mpv.el ends here
