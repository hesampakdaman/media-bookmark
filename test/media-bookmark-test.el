;;; media-bookmark-test.el --- Tests for media-bookmark -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The core is tested with a fake `test' backend that records the
;; calls it gets, and the mpv backend with a real mpv, if installed,
;; playing a short tone it encodes first.
;;
;; Run with:
;;
;;   emacs --batch -Q -L . -l test/media-bookmark-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'media-bookmark)
(require 'media-bookmark-mpv)

;;;; Fake backend

(defvar media-bookmark-test--calls nil
  "Calls to the `test' backend, most recent first.")

(cl-defmethod media-bookmark-backend-play ((_ (eql 'test)) session start)
  (push (list 'play (media-bookmark-session-file session) start)
        media-bookmark-test--calls))

(cl-defmethod media-bookmark-backend-seek ((_ (eql 'test)) _session time)
  (push (list 'seek time) media-bookmark-test--calls))

(cl-defmethod media-bookmark-backend-toggle-pause ((_ (eql 'test)) _session)
  (push (list 'toggle-pause) media-bookmark-test--calls))

(cl-defmethod media-bookmark-backend-stop ((_ (eql 'test)) session)
  (push (list 'stop) media-bookmark-test--calls)
  (media-bookmark-stopped session))

(defvar media-bookmark-test--players nil
  "Players the `test' backend can attach to.")

(cl-defmethod media-bookmark-backend-players ((_ (eql 'test)))
  media-bookmark-test--players)

(cl-defmethod media-bookmark-backend-attach ((_ (eql 'test)) _session handle)
  (push (list 'attach handle) media-bookmark-test--calls))

(cl-defmethod media-bookmark-backend-detach ((_ (eql 'test)) session)
  (push (list 'detach) media-bookmark-test--calls)
  (media-bookmark-stopped session))

(defmacro media-bookmark-test--with (&rest body)
  "Run BODY with no bookmarks, and the `test' backend by default."
  (declare (indent 0) (debug t))
  `(let* ((dir (make-temp-file "media-bookmark-test-" t))
          (bookmark-default-file (expand-file-name "bookmarks" dir))
          (bookmark-alist nil)
          (bookmark-bookmarks-timestamp nil)
          (bookmark-alist-modification-count 0)
          (bookmark-save-flag nil)
          (bookmark-automatically-show-annotations nil)
          (media-bookmark-backends (cons '(test . media-bookmark)
                                         media-bookmark-backends))
          (media-bookmark-backend 'test)
          (media-bookmark-update-bookmarks t)
          (media-bookmark-rewind-when-finished t)
          (media-bookmark-kill-buffer-when-stopped t)
          (media-bookmark-auto-bookmark nil)
          (media-bookmark-test--players nil)
          (media-bookmark-test--calls nil))
     (unwind-protect
         (progn ,@body)
       (dolist (buffer (buffer-list))
         (when (buffer-local-value 'media-bookmark--session buffer)
           (kill-buffer buffer)))
       (delete-directory dir t))))

(defun media-bookmark-test--session (buffer)
  "Return the session of BUFFER."
  (buffer-local-value 'media-bookmark--session buffer))

(defun media-bookmark-test--add (name file time)
  "Add a media bookmark NAME for FILE at TIME, with the `test' backend."
  (push `(,name (filename . ,file) (media-time . ,time)
                (media-backend . test) (handler . media-bookmark-jump))
        bookmark-alist))

(defun media-bookmark-test--time (name)
  "Return the media time of bookmark NAME."
  (bookmark-prop-get name 'media-time))

;;;; Core

(ert-deftest media-bookmark-test-set ()
  "\\[bookmark-set] in a session buffer records the position."
  (media-bookmark-test--with
    (let ((buffer (media-bookmark--play "/m/a.ogg" 'test nil)))
      (should (equal media-bookmark-test--calls '((play "/m/a.ogg" nil))))
      (media-bookmark-update (media-bookmark-test--session buffer) :time 61.6)
      (with-current-buffer buffer
        (bookmark-set "a")
        (should (equal bookmark-current-bookmark "a")))
      (should (equal (bookmark-get-filename "a") "/m/a.ogg"))
      (should (equal (media-bookmark-test--time "a") 62))
      (should (eq (bookmark-get-handler "a") 'media-bookmark-jump))
      (should (equal (bookmark-type-from-full-record (bookmark-get-bookmark "a"))
                     "Media")))))

(ert-deftest media-bookmark-test-default-name ()
  "The default bookmark name is the title, or else the file name."
  (media-bookmark-test--with
    (let ((buffer (media-bookmark--play "/m/a.ogg" 'test nil)))
      (with-current-buffer buffer
        (should (equal (car (bookmark-make-record)) "a.ogg"))
        (media-bookmark-update media-bookmark--session :title "A song")
        (should (equal (car (bookmark-make-record)) "A song"))
        (should (equal (buffer-name) "*media: A song*"))))))

(ert-deftest media-bookmark-test-jump ()
  "Jumping plays from the bookmark and makes it the session's."
  (media-bookmark-test--with
    (media-bookmark-test--add "a" "/m/a.ogg" 30)
    (bookmark-jump "a" #'ignore)
    (should (equal media-bookmark-test--calls '((play "/m/a.ogg" 30))))
    (let ((buffer (media-bookmark--find-buffer "/m/a.ogg")))
      (should (equal (buffer-local-value 'bookmark-current-bookmark buffer) "a"))
      (should (equal (media-bookmark-session-time
                      (media-bookmark-test--session buffer))
                     30)))))

(ert-deftest media-bookmark-test-jump-while-playing ()
  "Jumping to media that plays seeks, and saves the bookmark left."
  (media-bookmark-test--with
    (media-bookmark-test--add "a" "/m/a.ogg" 30)
    (media-bookmark-test--add "b" "/m/a.ogg" 500)
    (bookmark-jump "a" #'ignore)
    (media-bookmark-update (media-bookmark-test--session
                            (media-bookmark--find-buffer "/m/a.ogg"))
                           :time 45)
    (bookmark-jump "b" #'ignore)
    (should (equal media-bookmark-test--calls
                   '((seek 500) (play "/m/a.ogg" 30))))
    (should (equal (media-bookmark-test--time "a") 45))
    (should (equal (buffer-local-value 'bookmark-current-bookmark
                                       (media-bookmark--find-buffer "/m/a.ogg"))
                   "b"))))

(ert-deftest media-bookmark-test-follows-playback ()
  "The bookmark is updated on pause and stop, not while playing."
  (media-bookmark-test--with
    (media-bookmark-test--add "a" "/m/a.ogg" 30)
    (bookmark-jump "a" #'ignore)
    (let ((session (media-bookmark-test--session
                    (media-bookmark--find-buffer "/m/a.ogg"))))
      (media-bookmark-update session :time 40)
      (should (equal (media-bookmark-test--time "a") 30))
      (media-bookmark-update session :paused t)
      (should (equal (media-bookmark-test--time "a") 40))
      (should (= bookmark-alist-modification-count 1))
      (should (bookmark-prop-get "a" 'last-modified))
      (media-bookmark-update session :paused nil :time 90.4)
      (media-bookmark-stopped session)
      (should (equal (media-bookmark-test--time "a") 90))
      (should-not (media-bookmark-session-live session)))))

(ert-deftest media-bookmark-test-finished ()
  "Media played to the end is bookmarked at the start, if so configured."
  (dolist (rewind '(t nil))
    (media-bookmark-test--with
      (setq media-bookmark-rewind-when-finished rewind)
      (media-bookmark-test--add "a" "/m/a.ogg" 30)
      (bookmark-jump "a" #'ignore)
      (let ((session (media-bookmark-test--session
                      (media-bookmark--find-buffer "/m/a.ogg"))))
        (media-bookmark-update session :time 180)
        (media-bookmark-stopped session t)
        (should (equal (media-bookmark-test--time "a") (if rewind 0 180)))))))

(ert-deftest media-bookmark-test-no-update ()
  "With `media-bookmark-update-bookmarks' nil, bookmarks stay put."
  (media-bookmark-test--with
    (setq media-bookmark-update-bookmarks nil)
    (media-bookmark-test--add "a" "/m/a.ogg" 30)
    (bookmark-jump "a" #'ignore)
    (let ((session (media-bookmark-test--session
                    (media-bookmark--find-buffer "/m/a.ogg"))))
      (media-bookmark-update session :time 40 :paused t)
      (media-bookmark-stopped session)
      (should (equal (media-bookmark-test--time "a") 30)))))

(ert-deftest media-bookmark-test-other-bookmark-untouched ()
  "A session's bookmark for another file is not updated."
  (media-bookmark-test--with
    (media-bookmark-test--add "a" "/m/a.ogg" 30)
    (bookmark-jump "a" #'ignore)
    (let ((buffer (media-bookmark--find-buffer "/m/a.ogg")))
      (bookmark-set-filename "a" "/m/b.ogg")
      (media-bookmark-update (media-bookmark-test--session buffer) :time 99)
      (media-bookmark-stopped (media-bookmark-test--session buffer))
      (should (equal (media-bookmark-test--time "a") 30)))))

(ert-deftest media-bookmark-test-save-flag ()
  "Updates are written to disk according to `bookmark-save-flag'."
  (media-bookmark-test--with
    (setq bookmark-save-flag 1)
    (media-bookmark-test--add "a" "/m/a.ogg" 30)
    (bookmark-jump "a" #'ignore)
    (let ((session (media-bookmark-test--session
                    (media-bookmark--find-buffer "/m/a.ogg"))))
      (media-bookmark-update session :time 77 :paused t)
      (should (file-exists-p bookmark-default-file))
      (setq bookmark-alist nil)
      (bookmark-load bookmark-default-file t)
      (should (equal (media-bookmark-test--time "a") 77)))))

(ert-deftest media-bookmark-test-kill-buffer ()
  "Killing a session buffer updates the bookmark and stops the player."
  (media-bookmark-test--with
    (media-bookmark-test--add "a" "/m/a.ogg" 30)
    (bookmark-jump "a" #'ignore)
    (let ((buffer (media-bookmark--find-buffer "/m/a.ogg")))
      (media-bookmark-update (media-bookmark-test--session buffer) :time 50)
      (kill-buffer buffer)
      (should (equal (car media-bookmark-test--calls) '(stop)))
      (should (equal (media-bookmark-test--time "a") 50)))))

(ert-deftest media-bookmark-test-kill-emacs ()
  "Bookmarks of playing sessions are updated before Emacs saves them."
  (media-bookmark-test--with
    (media-bookmark-test--add "a" "/m/a.ogg" 30)
    (bookmark-jump "a" #'ignore)
    (media-bookmark-update (media-bookmark-test--session
                            (media-bookmark--find-buffer "/m/a.ogg"))
                           :time 65)
    (should (< (seq-position kill-emacs-hook
                             #'media-bookmark--update-all-bookmarks)
               (or (seq-position kill-emacs-hook #'bookmark-exit-hook-internal)
                   most-positive-fixnum)))
    (media-bookmark--update-all-bookmarks)
    (should (equal (media-bookmark-test--time "a") 65))))

(ert-deftest media-bookmark-test-commands ()
  "Session buffer commands drive the backend."
  (media-bookmark-test--with
    (setq media-bookmark-kill-buffer-when-stopped nil)
    (let ((buffer (media-bookmark--play "/m/a.ogg" 'test 100)))
      (with-current-buffer buffer
        (media-bookmark-toggle-pause)
        (media-bookmark-seek-forward 2)
        (media-bookmark-seek-backward)
        (media-bookmark-stop)
        (should (equal (nreverse media-bookmark-test--calls)
                       '((play "/m/a.ogg" 100) (toggle-pause)
                         (seek 120) (seek 110) (stop))))
        ;; Stopped: play again from where it stopped.
        (setq media-bookmark-test--calls nil)
        (media-bookmark-toggle-pause)
        (should (equal media-bookmark-test--calls '((play "/m/a.ogg" 110))))
        (should (string-match-p "Playing" (buffer-string)))))))

(ert-deftest media-bookmark-test-unknown-backend ()
  "An unregistered backend is reported, and the session is not left live."
  (media-bookmark-test--with
    (let ((err (should-error (media-bookmark--play "/m/a.ogg" 'nope 0)
                             :type 'user-error)))
      (should (string-match-p "nope" (cadr err))))
    (should-not (media-bookmark--find-buffer "/m/a.ogg"))))

(ert-deftest media-bookmark-test-normalize ()
  (should (equal (media-bookmark--normalize "https://e.com/v?x=1")
                 "https://e.com/v?x=1"))
  (should (equal (media-bookmark--normalize "/m/../m/a.ogg") "/m/a.ogg"))
  (should (equal (media-bookmark--format-time 65) "1:05"))
  (should (equal (media-bookmark--format-time 3725.4) "1:02:05")))

;;;; Closing and attaching

(ert-deftest media-bookmark-test-close-stopped-only ()
  "Only the buffer of the player that stopped is closed."
  (media-bookmark-test--with
    (media-bookmark-test--add "a" "/m/a.ogg" 30)
    (let* ((a (progn (bookmark-jump "a" #'ignore)
                     (media-bookmark--find-buffer "/m/a.ogg")))
           (b (media-bookmark--play "/m/b.ogg" 'test nil))
           (session (media-bookmark-test--session a)))
      (switch-to-buffer a)
      (media-bookmark-update session :time 44)
      (media-bookmark-stopped session)
      (should (equal (media-bookmark-test--time "a") 44))
      (should-not (buffer-live-p a))
      (should (buffer-live-p b))
      (should (media-bookmark-session-live (media-bookmark-test--session b))))))

(ert-deftest media-bookmark-test-keep-stopped ()
  "With `media-bookmark-kill-buffer-when-stopped' nil, the buffer stays."
  (media-bookmark-test--with
    (setq media-bookmark-kill-buffer-when-stopped nil)
    (let ((buffer (media-bookmark--play "/m/a.ogg" 'test nil)))
      (media-bookmark-stopped (media-bookmark-test--session buffer))
      (should (buffer-live-p buffer))
      (should (string-match-p "Stopped" (with-current-buffer buffer
                                          (buffer-string)))))))

(ert-deftest media-bookmark-test-attach ()
  "Attaching follows a running player, and killing detaches from it."
  (media-bookmark-test--with
    (media-bookmark-test--add "a" "/m/a.ogg" 30)
    (setq media-bookmark-test--players
          '((:file "/m/a.ogg" :title "A song" :handle h1)
            (:file "/m/b.ogg" :handle h2)))
    ;; Only the `test' backend, not any mpv running on this machine.
    (setq media-bookmark-backends '((test . media-bookmark)))
    (should (equal (media-bookmark--players)
                   '((:file "/m/a.ogg" :title "A song" :handle h1 :backend test)
                     (:file "/m/b.ogg" :handle h2 :backend test))))
    (media-bookmark-attach (car (media-bookmark--players)))
    (should (equal media-bookmark-test--calls '((attach h1))))
    (should (equal (buffer-name) "*media: A song*"))
    (should (equal bookmark-current-bookmark "a"))
    (should (media-bookmark-session-attached media-bookmark--session))
    ;; The position is the player's: no seek, and the bookmark follows.
    (media-bookmark-update media-bookmark--session :time 500 :paused t)
    (should (equal (media-bookmark-test--time "a") 500))
    (should-error (media-bookmark-attach (car (media-bookmark--players)))
                  :type 'user-error)
    (kill-buffer (current-buffer))
    (should (equal (car media-bookmark-test--calls) '(detach)))))

(ert-deftest media-bookmark-test-attach-auto ()
  "An attached player without a bookmark is bookmarked automatically."
  (media-bookmark-test--with
    (setq media-bookmark-auto-bookmark t)
    (media-bookmark-attach '(:file "/m/b.ogg" :handle h2 :backend test))
    (media-bookmark-update media-bookmark--session :time 12 :paused t)
    (should (equal (media-bookmark-test--time "b.ogg") 12))))

;;;; Automatic bookmarks

(defun media-bookmark-test--names ()
  "Return the names of all bookmarks."
  (mapcar #'car bookmark-alist))

(ert-deftest media-bookmark-test-auto-create ()
  "Automatic sessions are bookmarked when first saved, then updated."
  (media-bookmark-test--with
    (let* ((buffer (media-bookmark--play "/m/a.ogg" 'test nil t))
           (session (media-bookmark-test--session buffer)))
      (media-bookmark-update session :title "A song")
      (media-bookmark-update session :time 40)
      (should-not bookmark-alist)
      (media-bookmark-update session :paused t)
      (should (equal (media-bookmark-test--names) '("A song")))
      (should (equal (media-bookmark-test--time "A song") 40))
      (should (equal (buffer-local-value 'bookmark-current-bookmark buffer)
                     "A song"))
      (should (bookmark-prop-get "A song" 'last-modified))
      (media-bookmark-update session :paused nil :time 70)
      (media-bookmark-stopped session)
      (should (equal (media-bookmark-test--names) '("A song")))
      (should (equal (media-bookmark-test--time "A song") 70)))))

(ert-deftest media-bookmark-test-auto-not-at-start ()
  "Nothing is bookmarked at the start, or after playing to the end."
  (media-bookmark-test--with
    (let ((session (media-bookmark-test--session
                    (media-bookmark--play "/m/a.ogg" 'test nil t))))
      (media-bookmark-update session :time 0.3 :paused t)
      (media-bookmark-update session :time 180)
      (media-bookmark-stopped session t)
      (should-not bookmark-alist))))

(ert-deftest media-bookmark-test-auto-off ()
  "Without AUTO, or with `media-bookmark-update-bookmarks' nil, nothing is created."
  (media-bookmark-test--with
    (let ((session (media-bookmark-test--session
                    (media-bookmark--play "/m/a.ogg" 'test nil))))
      (media-bookmark-update session :time 40 :paused t)
      (should-not bookmark-alist))
    (setq media-bookmark-update-bookmarks nil)
    (let ((session (media-bookmark-test--session
                    (media-bookmark--play "/m/b.ogg" 'test nil t))))
      (media-bookmark-update session :time 40 :paused t)
      (should-not bookmark-alist))))

(ert-deftest media-bookmark-test-auto-unique-name ()
  "An automatic bookmark does not replace one of the same name."
  (media-bookmark-test--with
    (push '("a.ogg" (filename . "/elsewhere/a.ogg") (position . 1)) bookmark-alist)
    (let ((session (media-bookmark-test--session
                    (media-bookmark--play "/m/a.ogg" 'test nil t))))
      (media-bookmark-update session :time 40 :paused t)
      (should (equal (media-bookmark-test--names) '("a.ogg<2>" "a.ogg")))
      (should (equal (bookmark-get-filename "a.ogg") "/elsewhere/a.ogg")))))

(ert-deftest media-bookmark-test-play-resumes ()
  "`media-bookmark-play' resumes from the media's last modified bookmark."
  (media-bookmark-test--with
    (media-bookmark-test--add "old" "/m/a.ogg" 10)
    (bookmark-prop-set "old" 'last-modified '(1 0 0 0))
    (media-bookmark-test--add "new" "/m/a.ogg" 30)
    (bookmark-prop-set "new" 'last-modified '(2 0 0 0))
    (bookmark-prop-set "old" 'last-modified '(0 5 0 0))
    (media-bookmark-test--add "other" "/m/b.ogg" 50)
    (media-bookmark-play "/m/a.ogg")
    (should (equal media-bookmark-test--calls '((play "/m/a.ogg" 30))))
    (should (equal bookmark-current-bookmark "new"))
    (should (eq (current-buffer) (media-bookmark--find-buffer "/m/a.ogg")))
    ;; Already playing: just show it.
    (media-bookmark-play "/m/a.ogg")
    (should (equal media-bookmark-test--calls '((play "/m/a.ogg" 30))))))

(ert-deftest media-bookmark-test-play-new ()
  "`media-bookmark-play' plays media without bookmarks automatically."
  (media-bookmark-test--with
    (setq media-bookmark-auto-bookmark t)
    (media-bookmark-play "/m/a.ogg")
    (should (equal media-bookmark-test--calls '((play "/m/a.ogg" nil))))
    (should (media-bookmark-session-auto media-bookmark--session))
    (should (string-match-p "set when paused or stopped" (buffer-string)))))

(ert-deftest media-bookmark-test-play-auto-bookmark-nil ()
  "By default, media is only bookmarked by hand."
  (media-bookmark-test--with
    (media-bookmark-play "/m/a.ogg")
    (should (equal media-bookmark-test--calls '((play "/m/a.ogg" nil))))
    (should-not (media-bookmark-session-auto media-bookmark--session))
    (should (string-match-p "None; m sets one" (buffer-string)))
    (media-bookmark-update media-bookmark--session :time 40 :paused t)
    (should-not bookmark-alist)))

(ert-deftest media-bookmark-test-set-command ()
  "`media-bookmark-set' bookmarks playing media from any buffer."
  (media-bookmark-test--with
    (should-error (media-bookmark-set) :type 'user-error)
    (let* ((buffer (media-bookmark--play "/m/a.ogg" 'test nil))
           (session (media-bookmark-test--session buffer)))
      (media-bookmark-update session :time 61)
      (with-current-buffer buffer
        (should (string-match-p "m bookmark" (buffer-string))))
      (with-temp-buffer
        (cl-letf (((symbol-function 'read-from-minibuffer)
                   (lambda (&rest _) "manual")))
          (media-bookmark-set)))
      (should (equal (media-bookmark-test--time "manual") 61))
      (should (eq (bookmark-get-handler "manual") 'media-bookmark-jump))
      (should (equal (buffer-local-value 'bookmark-current-bookmark buffer)
                     "manual"))
      (with-current-buffer buffer
        (should (string-match-p "Bookmark  manual" (buffer-string)))))))

;;;; mpv

(defvar media-bookmark-test--media nil
  "A 20-second tone, encoded by mpv for the mpv tests.")

(defun media-bookmark-test--media ()
  "Return `media-bookmark-test--media', encoding it first if needed."
  (unless (and media-bookmark-test--media
               (file-exists-p media-bookmark-test--media))
    (let ((file (make-temp-file "media-bookmark-test-" nil ".ogg")))
      (should (zerop (call-process "mpv" nil nil nil "--no-config"
                                   "--really-quiet"
                                   "av://lavfi:sine=frequency=440:duration=20"
                                   (concat "--o=" file))))
      (setq media-bookmark-test--media file)))
  media-bookmark-test--media)

(defun media-bookmark-test--wait (predicate)
  "Wait up to 10 seconds for PREDICATE to return non-nil, and return that."
  (let ((deadline (+ (float-time) 10))
        value)
    (while (and (not (setq value (funcall predicate)))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (should value)
    value))

(defmacro media-bookmark-test--with-mpv (&rest body)
  "Run BODY as `media-bookmark-test--with', with a silent mpv backend."
  (declare (indent 0) (debug t))
  `(progn
     (skip-unless (executable-find "mpv"))
     (media-bookmark-test--with
       (let ((media-bookmark-backend 'mpv)
             (media-bookmark-mpv-arguments '("--no-config" "--ao=null"
                                             "--vo=null")))
         ,@body))))

(ert-deftest media-bookmark-test-mpv-resume ()
  "mpv starts from the bookmark, and quitting it updates the bookmark."
  (media-bookmark-test--with-mpv
    (let ((file (media-bookmark--normalize (media-bookmark-test--media))))
      (push `("tone" (filename . ,file) (media-time . 5) (media-backend . mpv)
              (handler . media-bookmark-jump))
            bookmark-alist)
      (bookmark-jump "tone" #'ignore)
      (let* ((buffer (media-bookmark--find-buffer file))
             (session (media-bookmark-test--session buffer)))
        (media-bookmark-test--wait
         (lambda () (>= (media-bookmark-session-time session) 6.5)))
        (should (equal (media-bookmark-session-title session)
                       (file-name-nondirectory file)))
        (should (< 19 (media-bookmark-session-duration session) 21))
        (with-current-buffer buffer
          (media-bookmark-toggle-pause))
        (media-bookmark-test--wait
         (lambda () (media-bookmark-session-paused session)))
        (should (<= 6 (media-bookmark-test--time "tone") 9))
        (with-current-buffer buffer
          (media-bookmark-seek-forward))
        (with-current-buffer buffer
          (media-bookmark-stop))
        (media-bookmark-test--wait
         (lambda () (not (media-bookmark-session-live session))))
        (should (<= 16 (media-bookmark-test--time "tone") 19))))))

(ert-deftest media-bookmark-test-mpv-finished ()
  "mpv playing to the end rewinds the bookmark."
  (media-bookmark-test--with-mpv
    (let ((file (media-bookmark--normalize (media-bookmark-test--media))))
      (push `("tone" (filename . ,file) (media-time . 18.5) (media-backend . mpv)
              (handler . media-bookmark-jump))
            bookmark-alist)
      (bookmark-jump "tone" #'ignore)
      (let ((session (media-bookmark-test--session
                      (media-bookmark--find-buffer file))))
        (media-bookmark-test--wait
         (lambda () (not (media-bookmark-session-live session))))
        (should (equal (media-bookmark-test--time "tone") 0))))))

(ert-deftest media-bookmark-test-mpv-kill-buffer ()
  "Killing the session buffer quits mpv."
  (media-bookmark-test--with-mpv
    (let* ((file (media-bookmark--normalize (media-bookmark-test--media)))
           (buffer (media-bookmark--play file 'mpv nil))
           (session (media-bookmark-test--session buffer))
           (process (media-bookmark-mpv--state-process
                     (media-bookmark-session-data session))))
      (media-bookmark-test--wait
       (lambda () (> (media-bookmark-session-time session) 0.5)))
      (kill-buffer buffer)
      (media-bookmark-test--wait (lambda () (not (process-live-p process)))))))

(defun media-bookmark-test--external-mpv (socket file)
  "Start an mpv outside media-bookmark on FILE, with IPC at SOCKET."
  (let ((process (start-process "external-mpv" nil "mpv" "--no-config"
                                "--ao=null" "--vo=null"
                                (concat "--input-ipc-server=" socket)
                                (expand-file-name file))))
    (set-process-query-on-exit-flag process nil)
    (media-bookmark-test--wait (lambda () (file-exists-p socket)))
    process))

(ert-deftest media-bookmark-test-mpv-attach ()
  "An mpv started outside Emacs is found, followed, and left running."
  (media-bookmark-test--with-mpv
    (setq media-bookmark-auto-bookmark t)
    (let* ((socket (expand-file-name "external.sock" dir))
           (media-bookmark-mpv-sockets nil)
           (file (media-bookmark--normalize (media-bookmark-test--media)))
           (process (media-bookmark-test--external-mpv socket file))
           (find (lambda ()
                   (cl-find socket (media-bookmark--players)
                            :key (lambda (p) (plist-get p :handle))
                            :test #'equal)))
           player)
      (unwind-protect
          (progn
            ;; mpv has no path until it has opened the file.
            (setq player
                  (media-bookmark-test--wait
                   (lambda ()
                     (or (funcall find)
                         (progn
                           (message "No player yet: sockets %S, mpv alive %S, query %S"
                                    (media-bookmark-mpv--sockets)
                                    (process-live-p process)
                                    (media-bookmark-mpv--query socket '("path")))
                           nil)))))
            (should (equal (media-bookmark--normalize (plist-get player :file))
                           file))
            (media-bookmark-attach player)
            (let ((buffer (current-buffer))
                  (session media-bookmark--session))
              (media-bookmark-test--wait
               (lambda () (> (media-bookmark-session-time session) 0.5)))
              ;; Our own session is not offered again.
              (should-not (funcall find))
              (kill-buffer buffer)
              (should (equal (media-bookmark-test--time
                              (file-name-nondirectory file))
                             (round (media-bookmark-session-time session))))
              (accept-process-output nil 0.3)
              (should (process-live-p process))
              ;; Attach again, then quit the player itself.
              (media-bookmark-attach (funcall find))
              (setq buffer (current-buffer))
              (media-bookmark-stop)
              (media-bookmark-test--wait (lambda () (not (process-live-p process))))
              (media-bookmark-test--wait (lambda () (not (buffer-live-p buffer))))))
        (delete-process process)))))

(ert-deftest media-bookmark-test-mpv-sockets ()
  "Sockets are read from options and mpv.conf, as mpv expands them."
  (let* ((home (make-temp-file "media-bookmark-test-mpv-" t))
         (process-environment (cons (concat "MPV_HOME=" home)
                                    process-environment))
         (media-bookmark-mpv-sockets '("/tmp/a.sock" "~~/b.sock")))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "mpv.conf" home)
            (insert "fs=yes\n  input-ipc-server = ~~/socket  \n"))
          (let ((sockets (media-bookmark-mpv--sockets)))
            (should (member "/tmp/a.sock" sockets))
            (should (member (expand-file-name "b.sock" home) sockets))
            (should (member (expand-file-name "socket" home) sockets))))
      (delete-directory home t))))

(ert-deftest media-bookmark-test-mpv-two-players ()
  "Quitting one of two players closes only its session."
  (media-bookmark-test--with-mpv
    (let* ((file (media-bookmark--normalize (media-bookmark-test--media)))
           (other (expand-file-name "copy.ogg" dir))
           (_ (copy-file file other))
           (a (media-bookmark--play file 'mpv nil t))
           (b (media-bookmark--play other 'mpv nil t))
           (session-a (media-bookmark-test--session a))
           (session-b (media-bookmark-test--session b)))
      (media-bookmark-test--wait
       (lambda () (and (> (media-bookmark-session-time session-a) 1.2)
                       (> (media-bookmark-session-time session-b) 1.2))))
      (with-current-buffer a
        (media-bookmark-stop))
      (media-bookmark-test--wait (lambda () (not (buffer-live-p a))))
      (accept-process-output nil 0.3)
      (should (buffer-live-p b))
      (should (media-bookmark-session-live session-b))
      (should (equal (mapcar #'car bookmark-alist)
                     (list (file-name-nondirectory file)))))))

(provide 'media-bookmark-test)
;;; media-bookmark-test.el ends here
