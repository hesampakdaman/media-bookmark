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
;;   M-x media-bookmark-set    bookmark the playing media
;;
;;   M-x media-bookmark-attach follow an mpv started outside Emacs
;;
;; Playing shows a session buffer for the media.  In it, m or
;; \\[bookmark-set] (C-x r m) bookmarks the position, and
;; \\[bookmark-jump] (C-x r b) resumes from any buffer; so does playing
;; the media again.  To bookmark media automatically, see
;; `media-bookmark-auto-bookmark'.  The session buffer is shown with
;; `pop-to-buffer', so `display-buffer-alist' decides where.
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

(defcustom media-bookmark-auto-bookmark nil
  "Non-nil means bookmark media without a bookmark automatically.
Such media, played with `media-bookmark-play' or attached to with
`media-bookmark-attach', is bookmarked under its title the first
time its position is saved, as for `media-bookmark-update-bookmarks',
unless that position is the start.  When nil, bookmark media with
`media-bookmark-set'."
  :type 'boolean)

(defcustom media-bookmark-rewind-when-finished t
  "Non-nil means media played to the end is bookmarked at the start."
  :type 'boolean)

(defcustom media-bookmark-kill-buffer-when-stopped t
  "Non-nil means close a session buffer when its player stops.
Only the buffer of the player that stopped is closed, after its
bookmark is updated.  When nil, the buffer stays, and
\\<media-bookmark-mode-map>\\[media-bookmark-toggle-pause] in it plays the media again."
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
FILE is a file name or URL.  TIME and DURATION are in seconds.  AUTO
non-nil means bookmark the media automatically.  ATTACHED non-nil
means the player was started outside Emacs.  DATA belongs to the
backend."
  backend file title (time 0) duration paused live buffer timer auto
  attached data)

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

(defun media-bookmark--play (file backend start &optional auto)
  "Play FILE with BACKEND from START seconds and return its session buffer.
START nil lets the player choose.  If FILE is already playing, seek
to START instead.  AUTO non-nil means bookmark FILE automatically."
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
                     :time (or start 0) :auto auto))
      (with-current-buffer buffer
        (setq media-bookmark--session session))
      (media-bookmark--start session start))
    (media-bookmark--redisplay session)
    buffer))

(defun media-bookmark--start (session start &optional player)
  "Start SESSION's player from START seconds, or where it chooses if nil.
If PLAYER is non-nil, attach to that running player instead; it is
one returned by `media-bookmark-backend-players'."
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
        (setf (media-bookmark-session-attached session) (and player t))
        (if player
            (media-bookmark-backend-attach
             (media-bookmark-session-backend session) session
             (plist-get player :handle))
          (media-bookmark-backend-play
           (media-bookmark-session-backend session) session start)))
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
;; what the player does with `media-bookmark-update',
;; `media-bookmark-switched' and `media-bookmark-stopped'; the core
;; takes care of buffers and bookmarks.
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

(cl-defgeneric media-bookmark-backend-players (backend)
  "Return the running players of BACKEND that a session can attach to.
Each is a plist with :file, the media it plays, :handle, which
`media-bookmark-backend-attach' gets, and optionally :title.
Implementing this, `media-bookmark-backend-attach' and
`media-bookmark-backend-detach' is optional; it lets
`media-bookmark-attach' follow players started outside Emacs."
  (ignore backend)
  nil)

(cl-defgeneric media-bookmark-backend-attach (backend session handle)
  "Make BACKEND follow the running player HANDLE for SESSION.
HANDLE is from `media-bookmark-backend-players'.  As with
`media-bookmark-backend-play', report the player's state with
`media-bookmark-update', and call `media-bookmark-stopped' when it
exits or is detached."
  (ignore session handle)
  (error "Backend `%s' does not implement `media-bookmark-backend-attach'"
         backend))

(cl-defgeneric media-bookmark-backend-detach (backend session)
  "Make BACKEND stop following the player of SESSION, but leave it running.
Call `media-bookmark-stopped' when done.  Used for attached players;
the default quits the player."
  (media-bookmark-backend-stop backend session))

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

(defun media-bookmark-switched (session file &optional finished)
  "Record that SESSION's player has moved on to FILE, a file name or URL.
Backends call this when the player plays other media, as in a
playlist.  FINISHED non-nil means the previous media played to the
end.  The previous media's bookmark is updated, and if FILE has a
media bookmark, it becomes SESSION's and playback resumes from it."
  (let ((file (media-bookmark--normalize file))
        (buffer (media-bookmark-session-buffer session)))
    (unless (equal file (media-bookmark-session-file session))
      (when (and finished media-bookmark-rewind-when-finished)
        (setf (media-bookmark-session-time session) 0))
      (media-bookmark--update-bookmark session)
      (setf (media-bookmark-session-file session) file
            (media-bookmark-session-title session) nil
            (media-bookmark-session-duration session) nil
            (media-bookmark-session-time session) 0)
      (let* ((bookmark (media-bookmark--find-bookmark file))
             (time (and bookmark (bookmark-prop-get bookmark 'media-time))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq bookmark-current-bookmark
                  (and bookmark (bookmark-name-from-full-record bookmark)))
            (rename-buffer (format "*media: %s*" (media-bookmark--name session))
                           t)))
        (when (and time (> time 0))
          (setf (media-bookmark-session-time session) time)
          (media-bookmark-backend-seek (media-bookmark-session-backend session)
                                       session time)))
      (media-bookmark--redisplay session))))

(defvar media-bookmark--killing nil
  "The session buffer being killed, if any.")

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
    (media-bookmark--redisplay session)
    (let ((buffer (media-bookmark-session-buffer session)))
      (when (and media-bookmark-kill-buffer-when-stopped
                 (buffer-live-p buffer)
                 (not (eq buffer media-bookmark--killing)))
        (if-let* ((window (get-buffer-window buffer t)))
            (quit-window t window)
          (kill-buffer buffer))))))

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

(defun media-bookmark--bookmark (session)
  "Return the bookmark of SESSION, or nil if it has none.
The bookmark is the one last set or jumped to in SESSION's buffer,
if it is still a media bookmark of SESSION's file."
  (let ((buffer (media-bookmark-session-buffer session)))
    (when-let* (((buffer-live-p buffer))
                (name (buffer-local-value 'bookmark-current-bookmark buffer))
                (bookmark (bookmark-get-bookmark name 'noerror))
                ((eq (bookmark-get-handler bookmark) 'media-bookmark-jump))
                ((equal (media-bookmark--normalize
                         (bookmark-get-filename bookmark))
                        (media-bookmark-session-file session))))
      bookmark)))

(defun media-bookmark--find-bookmark (file)
  "Return the media bookmark of FILE modified last, or nil if none."
  (bookmark-maybe-load-default-file)
  (let (found)
    (dolist (bookmark bookmark-alist)
      (when (and (eq (bookmark-get-handler bookmark) 'media-bookmark-jump)
                 (equal (media-bookmark--normalize
                         (bookmark-get-filename bookmark))
                        file)
                 (or (null found)
                     (time-less-p
                      (or (bookmark-prop-get found 'last-modified) 0)
                      (or (bookmark-prop-get bookmark 'last-modified) 0))))
        (setq found bookmark)))
    found))

(defun media-bookmark--unique-name (name)
  "Return NAME, or NAME<N> if a bookmark is already named NAME."
  (let ((candidate name)
        (n 1))
    (while (bookmark-get-bookmark candidate 'noerror)
      (setq candidate (format "%s<%d>" name (cl-incf n))))
    candidate))

(defun media-bookmark--update-bookmark (session)
  "Set the position of SESSION's bookmark to SESSION's time.
If SESSION has no bookmark and is bookmarked automatically, create
one, unless the time is the start."
  (let ((buffer (media-bookmark-session-buffer session))
        (time (round (media-bookmark-session-time session))))
    (when (and media-bookmark-update-bookmarks (buffer-live-p buffer))
      (if-let* ((bookmark (media-bookmark--bookmark session)))
          (unless (equal time (bookmark-prop-get bookmark 'media-time))
            (bookmark-prop-set bookmark 'media-time time)
            (bookmark-update-last-modified bookmark)
            ;; As `bookmark-store' does.
            (cl-incf bookmark-alist-modification-count)
            (bookmark-bmenu-surreptitiously-rebuild-list)
            (when (bookmark-time-to-save-p)
              (let ((inhibit-message t))
                (bookmark-save))))
        (when (and (media-bookmark-session-auto session) (> time 0))
          (with-current-buffer buffer
            (bookmark-maybe-load-default-file)
            (let ((record (media-bookmark-make-record)))
              (let ((inhibit-message t))
                ;; Also makes it the buffer's `bookmark-current-bookmark'.
                (bookmark-store (media-bookmark--unique-name (car record))
                                (cdr record) nil))
              (bookmark-update-last-modified bookmark-current-bookmark)
              (message "Bookmarked \"%s\" at %s" bookmark-current-bookmark
                       (media-bookmark--format-time time)))))))))

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
                         ("Bookmark" ,(cond
                                       ((media-bookmark--bookmark session)
                                        bookmark-current-bookmark)
                                       ((and (media-bookmark-session-auto session)
                                             media-bookmark-update-bookmarks)
                                        "None yet; set when paused or stopped")
                                       (t (substitute-command-keys
                                           "None; \\<media-bookmark-mode-map>\\[media-bookmark-set] sets one"))))))
            (insert (format "  %-9s %s\n" (car row) (cadr row))))
          (insert "\n"
                  (substitute-command-keys
                   "\\<media-bookmark-mode-map>\\[media-bookmark-toggle-pause] \
pause/play  \\[media-bookmark-seek-backward]/\\[media-bookmark-seek-forward] \
seek  \\[media-bookmark-set] bookmark  \\[media-bookmark-stop] stop  \\[quit-window] \
quit window")
                  "\n")
          (goto-char (min point (point-max))))))))

(defun media-bookmark--tick (session)
  "Redisplay SESSION if its buffer is visible."
  (when (get-buffer-window (media-bookmark-session-buffer session) t)
    (media-bookmark--redisplay session)))

(defun media-bookmark--kill-buffer ()
  "Update the bookmark of the current buffer's session and stop its player.
A player started outside Emacs is only detached from."
  (when-let* ((session media-bookmark--session)
              ((media-bookmark-session-live session)))
    (media-bookmark--update-bookmark session)
    (let ((media-bookmark--killing (current-buffer)))
      (if (media-bookmark-session-attached session)
          (media-bookmark-backend-detach
           (media-bookmark-session-backend session) session)
        (media-bookmark-backend-stop (media-bookmark-session-backend session)
                                     session)))))

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

(defun media-bookmark--read-session-buffer ()
  "Return the buffer of the playing media, read if there are several."
  (let ((buffers (cl-remove-if-not
                  (lambda (buffer)
                    (when-let* ((session (buffer-local-value
                                          'media-bookmark--session buffer)))
                      (media-bookmark-session-live session)))
                  (buffer-list))))
    (cond ((null buffers) (user-error "No media is playing"))
          ((null (cdr buffers)) (car buffers))
          (t (get-buffer (completing-read "Bookmark media: "
                                          (mapcar #'buffer-name buffers)
                                          nil t))))))

;;;###autoload
(defun media-bookmark-set ()
  "Bookmark the position of the playing media, prompting for a name.
In a session buffer, bookmark its media, as \\[bookmark-set] does.
Elsewhere, bookmark the media that is playing, read with completion
if there are several."
  (interactive)
  (let ((buffer (if media-bookmark--session
                    (current-buffer)
                  (media-bookmark--read-session-buffer))))
    (with-current-buffer buffer
      (call-interactively #'bookmark-set)
      (media-bookmark--redisplay media-bookmark--session))))

(defvar-keymap media-bookmark-mode-map
  "SPC" #'media-bookmark-toggle-pause
  "m" #'media-bookmark-set
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
Interactively, with a prefix argument, read a URL.

If FILE has a media bookmark, resume from it.  Otherwise, it is
bookmarked if `media-bookmark-auto-bookmark' is non-nil.  In the
session buffer, \\<media-bookmark-mode-map>\\[media-bookmark-set] \
bookmarks the playback position."
  (interactive
   (list (if current-prefix-arg
             (read-string "Play URL: " (thing-at-point 'url))
           (read-file-name "Play file: " nil nil t))))
  (let* ((file (media-bookmark--normalize file))
         (buffer (media-bookmark--find-buffer file))
         (bookmark (media-bookmark--find-bookmark file)))
    (cond
     ((and buffer (media-bookmark-session-live
                   (buffer-local-value 'media-bookmark--session buffer)))
      (pop-to-buffer buffer))
     (bookmark
      (bookmark-jump bookmark #'pop-to-buffer)
      (message "Resuming \"%s\" at %s" (bookmark-name-from-full-record bookmark)
               (media-bookmark--format-time
                (or (bookmark-prop-get bookmark 'media-time) 0))))
     (t
      (pop-to-buffer
       (media-bookmark--play file media-bookmark-backend nil
                             media-bookmark-auto-bookmark))))))

(defun media-bookmark--players ()
  "Return the running players of all backends, as for `media-bookmark-attach'.
Each is a plist from `media-bookmark-backend-players', with :backend."
  (cl-loop for (backend . _) in media-bookmark-backends
           when (ignore-errors (media-bookmark--load-backend backend) t)
           nconc (mapcar (lambda (player)
                           (plist-put (copy-sequence player) :backend backend))
                         (media-bookmark-backend-players backend))))

;;;###autoload
(defun media-bookmark-attach (player)
  "Follow PLAYER, a media player started outside Emacs.
Interactively, read PLAYER from the running players that backends
can attach to.  This shows a session buffer for its media, as
`media-bookmark-play' does, without changing the playback position.
The media's bookmark, if it has one, follows playback; if it has
none, see `media-bookmark-auto-bookmark'.  Killing the session buffer
stops following the player, but leaves it running."
  (interactive
   (let* ((players (or (media-bookmark--players)
                       (user-error "No player to attach to")))
          (choices (mapcar (lambda (player)
                             (cons (format "%s (%s)"
                                           (or (plist-get player :title)
                                               (plist-get player :file))
                                           (plist-get player :backend))
                                   player))
                           players)))
     (list (if (cdr choices)
               (cdr (assoc (completing-read "Attach to: " choices nil t)
                           choices))
             (cdar choices)))))
  (let* ((file (media-bookmark--normalize (plist-get player :file)))
         (buffer (media-bookmark--find-buffer file)))
    (when (and buffer (media-bookmark-session-live
                       (buffer-local-value 'media-bookmark--session buffer)))
      (user-error "Already following %s" file))
    (unless buffer
      (setq buffer (generate-new-buffer "*media*"))
      (with-current-buffer buffer
        (media-bookmark-mode)))
    (let ((session (media-bookmark--make-session
                    :backend (plist-get player :backend) :file file
                    :buffer buffer :title (plist-get player :title)
                    :auto media-bookmark-auto-bookmark)))
      (with-current-buffer buffer
        (setq media-bookmark--session session)
        (rename-buffer (format "*media: %s*" (media-bookmark--name session)) t)
        (when-let* ((bookmark (media-bookmark--find-bookmark file)))
          (setq bookmark-current-bookmark
                (bookmark-name-from-full-record bookmark))))
      (media-bookmark--start session nil player)
      (media-bookmark--redisplay session)
      (pop-to-buffer buffer))))

(provide 'media-bookmark)
;;; media-bookmark.el ends here
