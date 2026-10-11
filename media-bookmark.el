;;; media-bookmark.el --- Bookmark audio and video, resume where you left off -*- lexical-binding: t; -*-

;; Author: Hesam Pakdaman <https://github.com/hesampakdaman>
;; Maintainer: Hesam Pakdaman <https://github.com/hesampakdaman>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: multimedia, convenience
;; URL: https://github.com/hesampakdaman/media-bookmark
;; Assisted-by: Claude:claude-opus-5-5

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; media-bookmark makes audio and video files ordinary Emacs
;; bookmarks, the way PDF and EPUB readers do for documents: a
;; bookmark remembers the playback position, and jumping to it plays
;; the media from there.
;;
;; Usage:
;;
;;   M-x media-bookmark-play   play a file (with C-u, a URL) in a player
;;
;; This shows a session buffer for the playing media.  In it,
;; \\[bookmark-set] (C-x r m) bookmarks the current position, and
;; \\[bookmark-jump] (C-x r b) later resumes from it, from any buffer.
;;
;; While the media plays, the session's bookmark (the one it was set
;; or jumped from) follows playback: it is updated when you pause,
;; stop or quit the player, and when Emacs exits.  Bookmarks are
;; written to disk according to `bookmark-save-flag', as usual.  Set
;; `media-bookmark-update-bookmarks' to nil to update them only with
;; \\[bookmark-set].
;;
;; Players are backends, chosen with `media-bookmark-backend'.  An mpv
;; backend is included, in media-bookmark-mpv.el.  This file knows
;; nothing about particular players; see "Backend interface" below
;; for how to add one.

;;; Code:

(require 'bookmark)
(require 'cl-lib)

(defgroup media-bookmark nil
  "Bookmark audio and video, resume where you left off."
  :group 'bookmark
  :prefix "media-bookmark-")

(defcustom media-bookmark-backend 'mpv
  "Backend, that is player, used by `media-bookmark-play'.
It is a key of `media-bookmark-backends'.  A bookmark remembers the
backend it was made with."
  :type '(choice (const :tag "mpv" mpv)
                 (symbol :tag "Other backend")))

(defcustom media-bookmark-update-bookmarks t
  "Non-nil means a session's bookmark follows playback.
The bookmark is updated when the player pauses or stops, and when
Emacs exits.  It is written to disk according to `bookmark-save-flag'."
  :type 'boolean)

(defcustom media-bookmark-rewind-when-finished t
  "Non-nil means media played to the end is bookmarked at the start."
  :type 'boolean)

(defcustom media-bookmark-seek-step 10
  "Seconds that \\<media-bookmark-mode-map>\\[media-bookmark-seek-forward] \
and \\[media-bookmark-seek-backward] move."
  :type 'number)

;;;; Sessions

(cl-defstruct (media-bookmark-session
               (:constructor media-bookmark--make-session)
               (:copier nil))
  "Media playing, or played, in a session buffer.
FILE is a file name or URL.  TIME and DURATION are in seconds.  DATA
belongs to the backend."
  backend file title (time 0) duration paused live buffer timer data)

(defvar-local media-bookmark--session nil
  "The `media-bookmark-session' of a session buffer.")

(defun media-bookmark-url-p (file)
  "Return non-nil if FILE is a URL."
  (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*://" file))

(defun media-bookmark--normalize (file)
  "Return FILE as it is recorded in sessions and bookmarks."
  (if (media-bookmark-url-p file)
      file
    (abbreviate-file-name (expand-file-name file))))

(defun media-bookmark--name (session)
  "Return a name for SESSION's media."
  (or (media-bookmark-session-title session)
      (let ((file (media-bookmark-session-file session)))
        (if (media-bookmark-url-p file)
            file
          (file-name-nondirectory (directory-file-name file))))))

(defun media-bookmark--find-buffer (file)
  "Return the session buffer of FILE, or nil if there is none."
  (cl-find-if (lambda (buffer)
                (when-let* ((session (buffer-local-value
                                      'media-bookmark--session buffer)))
                  (equal file (media-bookmark-session-file session))))
              (buffer-list)))

(defun media-bookmark--play (file backend start)
  "Play FILE with BACKEND from START seconds and return its session buffer.
START nil lets the player choose.  If FILE is already playing, seek
to START instead."
  (let* ((buffer (media-bookmark--find-buffer file))
         (session (and buffer
                       (buffer-local-value 'media-bookmark--session buffer))))
    (if (and session (media-bookmark-session-live session))
        (progn
          ;; Save the position for the bookmark we are leaving.
          (media-bookmark--update-bookmark session)
          (when start
            (setf (media-bookmark-session-time session) start)
            (media-bookmark-backend-seek
             (media-bookmark-session-backend session) session start)))
      (unless buffer
        (setq buffer (generate-new-buffer
                      (format "*media: %s*" (media-bookmark--name
                                             (media-bookmark--make-session
                                              :file file)))))
        (with-current-buffer buffer
          (media-bookmark-mode)))
      (setq session (media-bookmark--make-session
                     :backend backend :file file :buffer buffer
                     :time (or start 0)))
      (with-current-buffer buffer
        (setq media-bookmark--session session))
      (media-bookmark--start session start))
    (media-bookmark--redisplay session)
    buffer))

(defun media-bookmark--start (session start)
  "Start SESSION's player from START seconds, or where it chooses if nil."
  (setf (media-bookmark-session-live session) t
        (media-bookmark-session-paused session) nil
        (media-bookmark-session-data session) nil
        (media-bookmark-session-timer session)
        (run-at-time 1 1 #'media-bookmark--tick session))
  ;; Run before `bookmark-exit-hook-internal' saves the bookmarks.
  (add-hook 'kill-emacs-hook #'media-bookmark--update-all-bookmarks -50)
  (condition-case err
      (progn
        (media-bookmark--load-backend (media-bookmark-session-backend session))
        (media-bookmark-backend-play
         (media-bookmark-session-backend session) session start))
    (error
     (media-bookmark-stopped session)
     (signal (car err) (cdr err)))))

;;;; Backend interface
;;
;; A backend connects sessions to a media player.  It is named by a
;; symbol and implements the four generic functions below with
;; methods specialized on (eql NAME); `media-bookmark-mpv' is an
;; example.  From the session, a backend reads the media with
;; `media-bookmark-session-file' (a file name, or a URL if
;; `media-bookmark-url-p') and keeps its own state, such as the
;; player process, in `media-bookmark-session-data'.  It reports
;; what the player does with `media-bookmark-update' and
;; `media-bookmark-stopped'; the core takes care of buffers and
;; bookmarks.
;;
;; Register the backend in `media-bookmark-backends' so that users
;; can choose it with `media-bookmark-backend'.

(defcustom media-bookmark-backends '((mpv . media-bookmark-mpv))
  "Available backends, as an alist of (NAME . FEATURE).
NAME is the symbol naming the backend, and FEATURE the library
defining it, which is loaded before the backend is first used."
  :type '(alist :key-type (symbol :tag "Backend")
                :value-type (symbol :tag "Feature")))

(defun media-bookmark--load-backend (backend)
  "Load the library of BACKEND, or signal an error if it is unknown."
  (let ((feature (alist-get backend media-bookmark-backends)))
    (unless feature
      (user-error "Unknown media-bookmark backend `%s'; see `media-bookmark-backends'"
                  backend))
    (require feature)))

(cl-defgeneric media-bookmark-backend-play (backend session start)
  "Start playing SESSION's file with BACKEND.
START is the position in seconds to start from, or nil to let the
player choose, for example to resume from its own saved position.
Return without waiting for the player; while it plays, report its
state with `media-bookmark-update', and when it exits, call
`media-bookmark-stopped'."
  (ignore session start)
  (error "Backend `%s' does not implement `media-bookmark-backend-play'"
         backend))

(cl-defgeneric media-bookmark-backend-seek (backend session time)
  "Make BACKEND's player for SESSION continue from TIME seconds."
  (ignore session time)
  (error "Backend `%s' does not implement `media-bookmark-backend-seek'"
         backend))

(cl-defgeneric media-bookmark-backend-toggle-pause (backend session)
  "Pause or resume BACKEND's player for SESSION.
Report the new state with `media-bookmark-update' and :paused."
  (ignore session)
  (error "Backend `%s' does not implement `media-bookmark-backend-toggle-pause'"
         backend))

(cl-defgeneric media-bookmark-backend-stop (backend session)
  "Quit BACKEND's player for SESSION.
Call `media-bookmark-stopped' when it has exited."
  (ignore session)
  (error "Backend `%s' does not implement `media-bookmark-backend-stop'"
         backend))

(defun media-bookmark-update (session &rest props)
  "Record the state of SESSION's player.
PROPS is a plist with any of :time, :duration and :title, and
:paused, which also updates SESSION's bookmark when non-nil."
  (when-let* ((time (plist-get props :time)))
    (setf (media-bookmark-session-time session) time))
  (when (plist-member props :duration)
    (setf (media-bookmark-session-duration session)
          (plist-get props :duration)))
  (when-let* ((title (plist-get props :title))
              ((not (equal title (media-bookmark-session-title session)))))
    (setf (media-bookmark-session-title session) title)
    (let ((buffer (media-bookmark-session-buffer session)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (rename-buffer (format "*media: %s*" title) t))))
    (media-bookmark--redisplay session))
  (when (plist-member props :paused)
    (let ((paused (plist-get props :paused)))
      (setf (media-bookmark-session-paused session) paused)
      (when paused
        (media-bookmark--update-bookmark session))
      (media-bookmark--redisplay session))))

(defun media-bookmark-stopped (session &optional finished)
  "Record that SESSION's player has stopped and update its bookmark.
FINISHED non-nil means the media played to the end."
  (when (media-bookmark-session-live session)
    (setf (media-bookmark-session-live session) nil
          (media-bookmark-session-paused session) nil)
    (when (timerp (media-bookmark-session-timer session))
      (cancel-timer (media-bookmark-session-timer session)))
    (when (and finished media-bookmark-rewind-when-finished)
      (setf (media-bookmark-session-time session) 0))
    (media-bookmark--update-bookmark session)
    (media-bookmark--redisplay session)))

;;;; Bookmarks

(defun media-bookmark-make-record ()
  "Return a bookmark record for the media of the current buffer.
This is `bookmark-make-record-function' in session buffers."
  (let ((session (media-bookmark--session)))
    `(,(media-bookmark--name session)
      (filename . ,(media-bookmark-session-file session))
      (media-time . ,(round (media-bookmark-session-time session)))
      (media-backend . ,(media-bookmark-session-backend session))
      (handler . media-bookmark-jump))))

;;;###autoload
(defun media-bookmark-jump (bookmark)
  "Play the media of BOOKMARK from its position.
If the media is already playing, seek to the position instead.
This is the handler of media bookmarks."
  (let* ((record (bookmark-get-bookmark bookmark))
         (buffer (media-bookmark--play
                  (media-bookmark--normalize (bookmark-get-filename record))
                  (or (bookmark-prop-get record 'media-backend)
                      media-bookmark-backend)
                  (or (bookmark-prop-get record 'media-time) 0))))
    (set-buffer buffer)
    (setq bookmark-current-bookmark (bookmark-name-from-full-record record))
    (media-bookmark--redisplay media-bookmark--session)))

;;;###autoload
(put 'media-bookmark-jump 'bookmark-handler-type "Media")

(defun media-bookmark--update-bookmark (session)
  "Set the position of SESSION's bookmark to SESSION's time.
The bookmark is the one last set or jumped to in SESSION's buffer."
  (let ((buffer (media-bookmark-session-buffer session)))
    (when-let* ((media-bookmark-update-bookmarks)
                ((buffer-live-p buffer))
                (name (buffer-local-value 'bookmark-current-bookmark buffer))
                (bookmark (bookmark-get-bookmark name 'noerror))
                ((eq (bookmark-get-handler bookmark) 'media-bookmark-jump))
                ((equal (media-bookmark--normalize
                         (bookmark-get-filename bookmark))
                        (media-bookmark-session-file session)))
                (time (round (media-bookmark-session-time session)))
                ((not (equal time (bookmark-prop-get bookmark 'media-time)))))
      (bookmark-prop-set bookmark 'media-time time)
      (bookmark-update-last-modified bookmark)
      ;; As `bookmark-store' does.
      (cl-incf bookmark-alist-modification-count)
      (bookmark-bmenu-surreptitiously-rebuild-list)
      (when (bookmark-time-to-save-p)
        (let ((inhibit-message t))
          (bookmark-save))))))

(defun media-bookmark--update-all-bookmarks ()
  "Update the bookmarks of all playing sessions."
  (dolist (buffer (buffer-list))
    (when-let* ((session (buffer-local-value 'media-bookmark--session buffer))
                ((media-bookmark-session-live session)))
      (media-bookmark--update-bookmark session))))

;;;; Session buffer

(defun media-bookmark--session ()
  "Return the session of the current buffer, or signal an error."
  (or media-bookmark--session
      (user-error "No media in this buffer")))

(defun media-bookmark--format-time (seconds)
  "Return SECONDS as H:MM:SS or M:SS."
  (let* ((seconds (max 0 (round seconds)))
         (h (/ seconds 3600))
         (m (/ (% seconds 3600) 60))
         (s (% seconds 60)))
    (if (> h 0)
        (format "%d:%02d:%02d" h m s)
      (format "%d:%02d" m s))))

(defun media-bookmark--redisplay (session)
  "Show the state of SESSION in its buffer."
  (let ((buffer (media-bookmark-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)
              (point (point))
              (duration (media-bookmark-session-duration session)))
          (erase-buffer)
          (insert (propertize (media-bookmark--name session) 'face 'bold)
                  "\n\n")
          (dolist (row `(("File" ,(media-bookmark-session-file session))
                         ("Position"
                          ,(concat (media-bookmark--format-time
                                    (media-bookmark-session-time session))
                                   (and duration
                                        (concat " / " (media-bookmark--format-time
                                                       duration)))))
                         ("State" ,(cond ((not (media-bookmark-session-live session))
                                          "Stopped")
                                         ((media-bookmark-session-paused session)
                                          "Paused")
                                         (t "Playing")))
                         ("Bookmark" ,(or bookmark-current-bookmark
                                          (substitute-command-keys
                                           "None; \\[bookmark-set] sets one")))))
            (insert (format "  %-9s %s\n" (car row) (cadr row))))
          (insert "\n"
                  (substitute-command-keys
                   "\\<media-bookmark-mode-map>\\[media-bookmark-toggle-pause] \
pause/play  \\[media-bookmark-seek-backward]/\\[media-bookmark-seek-forward] \
seek  \\[media-bookmark-stop] stop  \\[quit-window] quit window")
                  "\n")
          (goto-char (min point (point-max))))))))

(defun media-bookmark--tick (session)
  "Redisplay SESSION if its buffer is visible."
  (when (get-buffer-window (media-bookmark-session-buffer session) t)
    (media-bookmark--redisplay session)))

(defun media-bookmark--kill-buffer ()
  "Update the bookmark of the current buffer's session and stop its player."
  (when-let* ((session media-bookmark--session)
              ((media-bookmark-session-live session)))
    (media-bookmark--update-bookmark session)
    (media-bookmark-backend-stop (media-bookmark-session-backend session)
                                 session)))

(defun media-bookmark-toggle-pause ()
  "Pause or resume playback.
If the player has stopped, play again from where it stopped."
  (interactive nil media-bookmark-mode)
  (let ((session (media-bookmark--session)))
    (if (media-bookmark-session-live session)
        (media-bookmark-backend-toggle-pause
         (media-bookmark-session-backend session) session)
      (media-bookmark--start session (media-bookmark-session-time session))
      (media-bookmark--redisplay session))))

(defun media-bookmark-seek-forward (&optional n)
  "Seek N times `media-bookmark-seek-step' seconds forward."
  (interactive "p" media-bookmark-mode)
  (let* ((session (media-bookmark--session))
         (time (max 0 (+ (media-bookmark-session-time session)
                         (* (or n 1) media-bookmark-seek-step)))))
    (setf (media-bookmark-session-time session) time)
    (when (media-bookmark-session-live session)
      (media-bookmark-backend-seek (media-bookmark-session-backend session)
                                   session time))
    (media-bookmark--redisplay session)))

(defun media-bookmark-seek-backward (&optional n)
  "Seek N times `media-bookmark-seek-step' seconds backward."
  (interactive "p" media-bookmark-mode)
  (media-bookmark-seek-forward (- (or n 1))))

(defun media-bookmark-stop ()
  "Quit the player.  The session's bookmark keeps the position."
  (interactive nil media-bookmark-mode)
  (let ((session (media-bookmark--session)))
    (when (media-bookmark-session-live session)
      (media-bookmark-backend-stop (media-bookmark-session-backend session)
                                   session))))

(defvar-keymap media-bookmark-mode-map
  "SPC" #'media-bookmark-toggle-pause
  "f" #'media-bookmark-seek-forward
  "<right>" #'media-bookmark-seek-forward
  "b" #'media-bookmark-seek-backward
  "<left>" #'media-bookmark-seek-backward
  "s" #'media-bookmark-stop)

(define-derived-mode media-bookmark-mode special-mode "Media"
  "Major mode for media session buffers.
\\<media-bookmark-mode-map>
\\[bookmark-set] bookmarks the playback position.  While the media
plays, that bookmark follows playback; see
`media-bookmark-update-bookmarks'.

\\{media-bookmark-mode-map}"
  (setq-local bookmark-make-record-function #'media-bookmark-make-record)
  (setq-local bookmark-fringe-mark nil)
  (setq-local revert-buffer-function
              (lambda (&rest _)
                (media-bookmark--redisplay (media-bookmark--session))))
  (add-hook 'kill-buffer-hook #'media-bookmark--kill-buffer nil t))

;;;###autoload
(defun media-bookmark-play (file)
  "Play FILE, a media file or URL, and show its session buffer.
Interactively, with a prefix argument, read a URL.  In the session
buffer, \\[bookmark-set] bookmarks the playback position."
  (interactive
   (list (if current-prefix-arg
             (read-string "Play URL: " (thing-at-point 'url))
           (read-file-name "Play file: " nil nil t))))
  (pop-to-buffer
   (media-bookmark--play (media-bookmark--normalize file)
                         media-bookmark-backend nil)))

(provide 'media-bookmark)
;;; media-bookmark.el ends here
