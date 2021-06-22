
;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; If you get "gpg: Can't check signature: No public key"
;; Then:
;; 1. (setq package-check-signature nil)
;; 2. M-x package-install RET gnu-elpa-keyring-update RET
;; 3. remove 1.

;; Packages I use:
;;   better-defaults
;;   fill-column-indicator
;;   gnu-elpa-keyring-update
;;   rust-mode
;;   scad-mode

;; General
(setq initial-scratch-message "")
(setq inhibit-startup-message t)
(setf column-number-mode t)
(setf size-indication-mode t)
(add-hook 'latex-mode-hook (lambda () (electric-indent-mode -1)))
(add-hook 'latex-mode-hook 'flyspell-mode)


;; Adds mouse support in terminal
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mode (e))
  (setq mouse-sel-mode t))
(global-set-key [mouse-4] '(lambda ()
			     (interactive)
			     (scroll-down 1)))
(global-set-key [mouse-5] '(lambda ()
			     (interactive)
			     (scroll-up 1)))


;; Stop the backup files from being annoying
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5)   ; and how many of the old


;; Get those better defaults
(require 'better-defaults)
(save-place-mode 0) ;; disable save place


;; Turns on and fixes linum
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%3d |")


;; 80 column mode
(require 'fill-column-indicator)
(setq fci-rule-color "darkblue")
(setq fci-rule-column 81)
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

