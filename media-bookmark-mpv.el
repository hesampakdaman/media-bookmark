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
;; The mpv script mpv/media-bookmark.lua, installed with
;; `media-bookmark-mpv-install-script', gives mpv players started
;; elsewhere a socket, for `media-bookmark-attach', and binds Ctrl+b in
;; mpv to bookmark what it plays.
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

(defconst media-bookmark-mpv--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory of this library, with the mpv script in mpv/.")

(defcustom media-bookmark-mpv-socket-directory
  (expand-file-name "mpv-sockets" (or (getenv "XDG_RUNTIME_DIR")
                                      (getenv "TMPDIR")
                                      "/tmp"))
  "Directory of the IPC sockets made by the media-bookmark mpv script.
The script, which `media-bookmark-mpv-install-script' installs, gives
each mpv a socket here, where `media-bookmark-attach' finds it.  It
must match the directory in the script."
  :type 'directory)

(defcustom media-bookmark-mpv-sockets nil
  "IPC sockets of mpv players started outside Emacs.
`media-bookmark-attach' looks for players at these sockets, in
`media-bookmark-mpv-socket-directory', at those given to running mpv
processes with --input-ipc-server, and at the one set by
input-ipc-server in mpv.conf."
  :type '(repeat file))

(cl-defstruct (media-bookmark-mpv--state
               (:constructor media-bookmark-mpv--make-state)
               (:copier nil))
  "State of an mpv process and its IPC connection.
PROCESS is nil for an mpv started outside Emacs.  DIRECTORY is mpv's
working directory, against which it reports relative file names.
LOADING is non-nil while mpv loads a file, when it cannot seek yet;
SEEK is then the position to seek to once it has loaded."
  process connection socket (pending "") finished directory loading seek)

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
             (let ((process (media-bookmark-mpv--state-process mpv)))
               (or (null process) (process-live-p process))))
    (condition-case nil
        (progn
          (setf (media-bookmark-mpv--state-connection mpv)
                (make-network-process
                 :name "media-bookmark-mpv-ipc"
                 :family 'local
                 :service (media-bookmark-mpv--state-socket mpv)
                 :coding 'utf-8
                 :noquery t
                 :sentinel (lambda (connection _event)
                             (media-bookmark-mpv--connection-sentinel
                              session mpv connection))
                 :filter (lambda (_process string)
                           (media-bookmark-mpv--filter session mpv string))))
          ;; The quit command closes the socket before we could ask
          ;; for the position, so follow the position as it changes.
          ;; mpv reports changes in this order, so the working
          ;; directory and file come before the rest for that file.
          (cl-loop for property in '("working-directory" "path" "time-pos"
                                     "duration" "pause" "media-title")
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
         ("working-directory"
          (setf (media-bookmark-mpv--state-directory mpv) data))
         ("path"
          (when data
            (media-bookmark-switched
             session
             (if (media-bookmark-url-p data)
                 data
               (expand-file-name data (media-bookmark-mpv--state-directory mpv)))
             (media-bookmark-mpv--state-finished mpv))
            (setf (media-bookmark-mpv--state-finished mpv) nil)))
         ("time-pos" (when data (media-bookmark-update session :time data)))
         ("duration" (when data (media-bookmark-update session :duration data)))
         ("pause" (media-bookmark-update session :paused data))
         ("media-title" (when data (media-bookmark-update session :title data))))))
    ("end-file"
     (when (equal (plist-get message :reason) "eof")
       (setf (media-bookmark-mpv--state-finished mpv) t)))
    ("client-message"
     ;; Sent by mpv/media-bookmark.lua, with a key in mpv's window.
     (when (equal (append (plist-get message :args) nil)
                  '("media-bookmark" "bookmark"))
       ;; mpv names the script media_bookmark.
       (media-bookmark-mpv--send mpv "script-message-to" "media_bookmark"
                                 "bookmarked"
                                 (media-bookmark-bookmark-requested session))))
    ("start-file"
     (setf (media-bookmark-mpv--state-loading mpv) t))
    ("file-loaded"
     (setf (media-bookmark-mpv--state-loading mpv) nil)
     (when-let* ((time (media-bookmark-mpv--state-seek mpv)))
       (setf (media-bookmark-mpv--state-seek mpv) nil)
       (media-bookmark-mpv--send mpv "seek" time "absolute")))))

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

(defun media-bookmark-mpv--connection-sentinel (session mpv connection)
  "Stop SESSION when CONNECTION to MPV closes, if mpv is not ours.
For our own mpv, its process sentinel stops SESSION instead."
  (unless (or (process-live-p connection)
              (media-bookmark-mpv--state-process mpv))
    (when (eq mpv (media-bookmark-session-data session))
      (media-bookmark-stopped session (media-bookmark-mpv--state-finished mpv)))))

;;;; Players started outside Emacs

(defun media-bookmark-mpv--config-directory ()
  "Return mpv's configuration directory."
  (or (getenv "MPV_HOME")
      (expand-file-name "mpv" (or (getenv "XDG_CONFIG_HOME") "~/.config"))))

(defun media-bookmark-mpv--expand (file)
  "Expand FILE as mpv does, where ~~/ is mpv's configuration directory."
  (if (string-prefix-p "~~/" file)
      (expand-file-name (substring file 3) (media-bookmark-mpv--config-directory))
    (expand-file-name file)))

(defun media-bookmark-mpv--sockets ()
  "Return the IPC sockets where mpv players started outside Emacs may be."
  (let ((sockets (mapcar #'media-bookmark-mpv--expand media-bookmark-mpv-sockets))
        (config (expand-file-name "mpv.conf" (media-bookmark-mpv--config-directory))))
    (when (file-directory-p media-bookmark-mpv-socket-directory)
      (dolist (socket (directory-files media-bookmark-mpv-socket-directory
                                       t "\\`[^.]"))
        (push socket sockets)))
    (dolist (pid (list-system-processes))
      (let* ((attributes (process-attributes pid))
             (args (alist-get 'args attributes)))
        (when (and args
                   (equal (alist-get 'comm attributes) "mpv")
                   (string-match "--input-ipc-server[= ]\\([^ ]+\\)" args))
          (push (media-bookmark-mpv--expand (match-string 1 args)) sockets))))
    (when (file-readable-p config)
      (with-temp-buffer
        (insert-file-contents config)
        (while (re-search-forward
                "^[ \t]*input-ipc-server[ \t]*=[ \t]*\\(.*[^ \t\n]\\)" nil t)
          (push (media-bookmark-mpv--expand (match-string 1)) sockets))))
    (delete-dups (nreverse sockets))))

(defun media-bookmark-mpv--query (socket properties)
  "Return the values of PROPERTIES of the mpv at SOCKET.
Return nil if no mpv answers there within a second."
  (let* ((replies (make-hash-table))
         (pending "")
         (connection
          (condition-case nil
              (make-network-process
               :name "media-bookmark-mpv-query"
               :family 'local
               :service socket
               :coding 'utf-8
               :noquery t
               :sentinel #'ignore
               :filter
               (lambda (_process string)
                 (let ((lines (split-string (concat pending string) "\n")))
                   (setq pending (car (last lines)))
                   (dolist (line (butlast lines))
                     (when-let* ((reply (ignore-errors
                                          (json-parse-string
                                           line :object-type 'plist
                                           :null-object nil :false-object nil)))
                                 (id (plist-get reply :request_id)))
                       (puthash id (plist-get reply :data) replies))))))
            (file-error nil))))
    (when connection
      (unwind-protect
          (progn
            (cl-loop for property in properties
                     for id from 1
                     do (process-send-string
                         connection
                         (concat (json-serialize
                                  (list :command (vector "get_property" property)
                                        :request_id id))
                                 "\n")))
            (let ((deadline (+ (float-time) 1)))
              (while (and (< (hash-table-count replies) (length properties))
                          (< (float-time) deadline)
                          (process-live-p connection))
                (accept-process-output connection 0.05)))
            (when (= (hash-table-count replies) (length properties))
              (cl-loop for id from 1 to (length properties)
                       collect (gethash id replies))))
        (delete-process connection)))))

;;;###autoload
(defun media-bookmark-mpv-install-script ()
  "Install the mpv script that lets `media-bookmark-attach' find mpv.
Copy media-bookmark.lua to mpv's scripts directory.  With it, each
mpv started afterwards gets an IPC socket in
`media-bookmark-mpv-socket-directory'."
  (interactive)
  (let ((source (expand-file-name "mpv/media-bookmark.lua"
                                  media-bookmark-mpv--directory))
        (target (expand-file-name "scripts/media-bookmark.lua"
                                  (media-bookmark-mpv--config-directory))))
    (make-directory (file-name-directory target) t)
    (copy-file source target t)
    (message "Installed %s; mpv players started from now on can be attached to"
             (abbreviate-file-name target))))

(cl-defmethod media-bookmark-backend-players ((_backend (eql 'mpv)))
  "Return the mpv players started outside Emacs that can be attached to."
  (let ((own (cl-loop for buffer in (buffer-list)
                      for session = (buffer-local-value
                                     'media-bookmark--session buffer)
                      when (and session
                                (media-bookmark-session-live session)
                                (eq (media-bookmark-session-backend session) 'mpv)
                                (media-bookmark-session-data session))
                      collect (media-bookmark-mpv--state-socket
                               (media-bookmark-session-data session))))
        players)
    (dolist (socket (media-bookmark-mpv--sockets))
      (unless (member socket own)
        (pcase (media-bookmark-mpv--query
                socket '("path" "working-directory" "media-title"))
          (`(,(and (pred stringp) path) ,directory ,title)
           (push (list :file (if (media-bookmark-url-p path)
                                 path
                               (expand-file-name path directory))
                       :title title
                       :handle socket)
                 players)))))
    (nreverse players)))

(cl-defmethod media-bookmark-backend-attach ((_backend (eql 'mpv)) session socket)
  "Follow the mpv at SOCKET for SESSION."
  (let ((mpv (media-bookmark-mpv--make-state :socket socket)))
    (setf (media-bookmark-session-data session) mpv)
    (media-bookmark-mpv--connect session mpv 0)
    (unless (process-live-p (media-bookmark-mpv--state-connection mpv))
      (error "Cannot connect to mpv at %s" socket))))

(cl-defmethod media-bookmark-backend-detach ((_backend (eql 'mpv)) session)
  "Stop following the mpv of SESSION, leaving it running if not ours."
  (let ((mpv (media-bookmark-session-data session)))
    (if (media-bookmark-mpv--state-process mpv)
        (cl-call-next-method)
      (when-let* ((connection (media-bookmark-mpv--state-connection mpv)))
        (delete-process connection))
      (media-bookmark-stopped session))))

;;;; Commands

(cl-defmethod media-bookmark-backend-seek ((_backend (eql 'mpv)) session time)
  "Seek mpv playing SESSION to TIME."
  (let ((mpv (media-bookmark-session-data session)))
    (if (media-bookmark-mpv--state-loading mpv)
        (setf (media-bookmark-mpv--state-seek mpv) time)
      (media-bookmark-mpv--send mpv "seek" time "absolute"))))

(cl-defmethod media-bookmark-backend-toggle-pause ((_backend (eql 'mpv)) session)
  "Pause or resume mpv playing SESSION."
  (media-bookmark-mpv--send (media-bookmark-session-data session)
                            "cycle" "pause"))

(cl-defmethod media-bookmark-backend-stop ((_backend (eql 'mpv)) session)
  "Quit mpv playing SESSION."
  (let ((mpv (media-bookmark-session-data session)))
    (cond
     ((process-live-p (media-bookmark-mpv--state-connection mpv))
      (media-bookmark-mpv--send mpv "quit"))
     ((media-bookmark-mpv--state-process mpv)
      (delete-process (media-bookmark-mpv--state-process mpv)))
     (t (media-bookmark-stopped session)))))

(provide 'media-bookmark-mpv)
;;; media-bookmark-mpv.el ends here
