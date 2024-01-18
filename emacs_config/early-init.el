;; Everybody seems to do this and pair it with garbaage collection hack mode.
;; WARNING: Don't uncomment it unless gcmh-mode is turned on.
(setq gc-cons-threshold most-positive-fixnum)

;; Spacemacs does this, not sure why. Might add it...
;; (setq package-enable-at-startup nil)

;; Don't load site files. I like my config better
(setq site-run-file nil)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Unset `file-name-handler-alist' too (temporarily). Every file opened and
;; loaded by Emacs will run through this list to check for a proper handler for
;; the file, but during startup, it won’t need any of them.
(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old)))
