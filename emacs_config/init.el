;; TODO for this file
;; * look up tree sitter, everyone seems to love it
;; * find an automatic spell checker that works like the one I use in VSC
;; * trailing whitespace thing from doom

;; Inspiration/Sources
;; * http://pragmaticemacs.com/
;; * https://www.emacswiki.org/emacs/DotEmacsChallenge
;; * https://github.com/pavpanchekha/dotfiles
;; * https://github.com/ashton314/newbie.el
;; * https://git.sr.ht/~ashton314/emacs-bedrock
;; * https://github.com/mnewt/dotemacs
;; * https://github.com/doomemacs/doomemacs
;; * https://github.com/syl20bnr/spacemacs

;; Things I always forge
;; * `C-h k` is "describe next action"


;; +---------------------------------------------------------------------------+
;; | Custom functions                                                          |
;; +---------------------------------------------------------------------------+

;; Based on https://blog.lambda.cx/posts/emacs-align-columns
(defun align-non-space (BEG END)
  "Aligns non-space columns seperated by 2 or more spaces"
  (interactive "r")
  (align-regexp BEG END "\\(\\s-\\s-\\s-*\\)\\S-+" 1 2 t))

;; Tell how to kill emacs
(defun dont-kill-emacs ()
  (interactive)
  (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))


;; +---------------------------------------------------------------------------+
;; | Configuration of emacs itself                                             |
;; +---------------------------------------------------------------------------+

;; Shunt off generated code
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file)

;; Make emacs harder to exit than vim
(global-set-key "\C-x\C-c" 'dont-kill-emacs)

;; Smoother scrolling in gui
(when (display-graphic-p) (pixel-scroll-precision-mode))

;; Minimum width for line numbers
(setq-default display-line-numbers-width 3)

;; Don't use tabs
;; TODO: What about files already using tabs?
(setq-default indent-tabs-mode nil)

;; Set fill column to be 80
(setq-default display-fill-column-indicator-column 80)

;; Don't use stale files
(setq-default load-prefer-newer t)

;; 5MB ought to be enough for anyon
(setq-default undo-limit 5242880)

;; Don't make `#file` files
(setq-default create-lockfiles nil)

;; Silence bell
(setq-default ring-bell-function 'ignore)

;; Variables from startup.el
(setq-default
 auto-save-file-name-transforms '((".*" "~/.emacs.d/autosave/" t))
 auto-save-list-file-prefix "~/.emacs.d/autosave/"
 inhibit-default-init t
 inhibit-splash-screen t
 inhibit-startup-message t
 inhibit-startup-screen t
 initial-major-mode 'fundamental-mode
 initial-scratch-message "")

;; Variables from paragraph.el
(setq-default sentence-end-double-space nil)

;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://test.melpa.org/packages/") t)
(package-initialize)

;; Install use-package, then it will install everything else
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Always install packages if not found, dunno what the second one does
(setq use-package-always-ensure t)
;;(setq use-package-expand-minimally t)

;; Programming defaults
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Text defaults
(add-hook 'text-mode-hook 'visual-line-mode)

;; Keybindings
(use-package bind-key
  :config
  (bind-key "C--" 'undo)
  )


;; +---------------------------------------------------------------------------+
;; | Builtin packages                                                          |
;; +---------------------------------------------------------------------------+

;; Fix auto revert mode
;; TODO: Might require investigating...
;; may need auto-revert-notify-exclude-dir-regexp
(use-package autorevert
  :delight auto-revert-mode
  :custom
  (auto-revert-avoid-polling t)
  (auto-revert-interval 5)
  (auto-revert-check-vc-info t))

;; Make vertical seperator pretty
(use-package disp-table
  :config (set-display-table-slot standard-display-table 5 ? ))

;; Things from files.el
(use-package files
  :custom
  (require-final-newline t)
  (backup-directory-alist '(("." . "~/.emacs.d/backup")))
  (backup-by-copying t)               ; Don't clobber symlinks
  (backup-by-copying-when-linked t)   ; Don't break multiple hardlinks
  (backup-by-copying-when-mismatch t) ; Preserve owner/groups
  (version-control t)                 ; Use version numbers on backups
  (delete-old-versions t)             ; Automatically delete excess backups
  (kept-new-versions 20)              ; How many of the newest versions to keep
  (kept-old-versions 5))              ; and how many of the old

;; Autocomplete-like thing
(use-package ido
  :config (ido-mode t)
  :custom
  (ido-enable-flex-matching t))

;; Add mouse support in terminal
(use-package mouse
  :unless (display-graphic-p)
  :config
  (xterm-mouse-mode t)
  (global-unset-key (kbd "<C-down-mouse-1>")))

;; ...and the wheel/touchpad
(use-package mwheel
  :unless (display-graphic-p)
  :config (mouse-wheel-mode t)
  :custom (mouse-wheel-tilt-scroll t))

;; Simple settings
(use-package simple
  :config
  (column-number-mode 1) ;; Put column number in the bottom bar
  (show-paren-mode 1)) ;; Highlight matching parens

;; If performance dies, turn off slow things
(use-package so-long
  :config (global-so-long-mode 1))

;; Don't display CPU load in the mode line
(use-package time :custom (display-time-default-load-average nil))

;; Disable tool bar (gui thing)
(use-package tool-bar :config (tool-bar-mode -1))

;; Remote file editing
(use-package tramp)

;; Give better names when opening the same file in diff dirs
(use-package uniquify :custom (uniquify-buffer-name-style 'forward))

;; Backup in vc
(use-package vc-hooks :custom (vc-make-backup-files t))

;; Move through windows with meta-<arrow keys>
(use-package windmove :config (windmove-default-keybindings 'meta))


;; +---------------------------------------------------------------------------+
;; | External packages                                                         |
;; +---------------------------------------------------------------------------+

;; Uses system clipboard from terminal
(use-package clipetty
  :unless (display-graphic-p)
  :config (clipetty-mode 1))

;; Change minor mode names
(use-package delight)

;; Garbage collection
(use-package gcmh
  :delight
  :demand t
  :config (gcmh-mode 1))

;; Magit
(use-package magit)

;; Multiple cursors!
(use-package multiple-cursors :bind ("C-c C-SPC" . mc/edit-lines))

;; Show keybindings as autocomplete
(use-package which-key :config (which-key-mode 1))


;; +---------------------------------------------------------------------------+
;; | Languages                                                                 |
;; +---------------------------------------------------------------------------+

;; Markdown
(use-package markdown-mode)

;; Open SCAD
(use-package scad-mode)

;; Verilog
(use-package verilog-mode)


;; +---------------------------------------------------------------------------+
;; | Themes                                                                    |
;; +---------------------------------------------------------------------------+

;; Turn on powerline
(use-package airline-themes :config (load-theme 'airline-angr t))

;; Use a matching text theme
(use-package afternoon-theme :config (load-theme 'afternoon t))
