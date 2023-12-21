;; Don't use this config if emacs version is below 29.1
(if (version< emacs-version "29.1")
    (with-current-buffer " *load*"
      (goto-char (point-max))))

;; Shunt off generated code
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file)

;; MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; My added packages
(setq packages `(
   afternoon-theme
   airline-themes
   markdown-mode
   multiple-cursors
   powerline
   rust-mode
   scad-mode
   ))

;; Maybe refresh packages
(unless package-archive-contents (package-refresh-contents))

;; Install packages that are not on this machine
(dolist (package packages)
  (unless (package-installed-p package) (package-install package)))

;; Autocomplete-like thing
(ido-mode t)
(setq ido-enable-flex-matching t)

;; Remove menu bar
(menu-bar-mode -1)

;; Give better names when opening the same file in diff dirs
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Highlight matching parens
(show-paren-mode 1)

;; Don't use tabs (what about files already using tabs?)
(setq-default indent-tabs-mode nil)

;; Force newline at end of file
(setq require-final-newline t)

;; Don't use stale files
(setq load-prefer-newer t)

;; Stop the backup files from being annoying
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5)   ; and how many of the old

;; Based on https://blog.lambda.cx/posts/emacs-align-columns
(defun align-non-space (BEG END)
  "Aligns non-space columns seperated by 2 or more spaces"
  (interactive "r")
  (align-regexp BEG END "\\(\\s-\\s-\\s-*\\)\\S-+" 1 2 t))

;; Programming defaults
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Things to include with markdown mode
(add-hook 'markdown-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'markdown-mode-hook 'display-line-numbers-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; Latex customizations
(add-hook 'latex-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'latex-mode-hook 'display-line-numbers-mode)
(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'latex-mode-hook (lambda () (electric-indent-mode -1)))

;; Things to include with markdown mode
(add-hook 'rust-mode-hook (setq display-fill-column-indicator-column 100))

;; Things to include with markdown mode
(add-to-list 'auto-mode-alist '("\\.fpcore\\'" . scheme-mode))

;; Use UTF-8 for latex symbols
(setq org-pretty-entities t)

;; Hide formatting markers, like _underline_
(setf org-hide-emphasis-markers t)

;; Things to include with org mode
(add-hook 'org-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

;; Use a matching text theme
(require 'afternoon-theme)
(load-theme 'afternoon t)

;; Turn on powerline
(require 'airline-themes)
(load-theme 'airline-angr t)

;; Put column number in the bottom bar
(column-number-mode t)

;; Skip startup screen
(setf inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Silence bell
(setq ring-bell-function 'ignore)

;; Set fill column to be 80
(setq-default display-fill-column-indicator-column 80)

;; Don't soft wrap lines
(set-default 'truncate-lines t)

;; Scratch buffer
;; maybe set to journal if ~/Dropbox is present...
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setq initial-major-mode 'fundamental-mode)

;; Preserve hard links to the file you’re editing
;; (this is especially important if you edit system files).
(setq backup-by-copying-when-linked t)

;; Preserve the owner and group of the file you’re editing
;; (this is especially important if you edit files as root).
(setq backup-by-copying-when-mismatch t)

;; Always show trailing whitespace
(setq-default show-trailing-whitespace t)

;; multiple cursors!
(use-package multiple-cursors
  :bind ("C-c C-SPC" . mc/edit-lines))

;; Configurations only for the terminal version
(unless (display-graphic-p)

;; Adds mouse support in terminal
(require 'mwheel)
(require 'mouse)
(xterm-mouse-mode t)
(mouse-wheel-mode t)

;; Make vertical seperator pretty
;; (set-display-table-slot standard-display-table 5 ?│)
(set-display-table-slot standard-display-table 5 ? )
;;(set-face-background 'vertical-border "gray")
;;(set-face-foreground 'vertical-border (face-background 'vertical-border))

)

;; Configurations only for the GUI version
(when (display-graphic-p)

;; Remove GUI only top bar
(tool-bar-mode -1)

;; Muscle memory from terminal, which can't differentiate C-- and C-_
(bind-key "C--" 'undo)

;; Smoother scrolling
(pixel-scroll-precision-mode)

)
