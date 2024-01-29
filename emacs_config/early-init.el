;; Everybody seems to do this and pair it with garbaage collection hack mode.
;; WARNING: Don't uncomment it unless gcmh-mode is turned on.
(setq gc-cons-threshold most-positive-fixnum)

;; Don't load site files. I like my config better
(setq site-run-file nil)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
