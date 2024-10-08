;;; init.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Code:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; packages here
(straight-use-package 'use-package)
(straight-use-package 'smex)
(straight-use-package 'company)
(straight-use-package 'magit)
(straight-use-package 'diminish)
(straight-use-package 'rg)
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(straight-use-package 'move-text)
(straight-use-package 'swiper)
(straight-use-package 'ivy)
(straight-use-package 'counsel)
(straight-use-package 'ivy-hydra)
(straight-use-package 'solarized-theme)
(straight-use-package 'evil)
(straight-use-package 'goto-chg)
(straight-use-package 'topsy)
;; language major modes
(straight-use-package 'racket-mode)

;; https://git.sr.ht/~technomancy/better-defaults
(progn
  (ido-mode t)
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)

  (unless (memq window-system '(mac ns))
    (menu-bar-mode -1))
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (when (fboundp 'horizontal-scroll-bar-mode)
    (horizontal-scroll-bar-mode -1))

  (autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR." t)

  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)

  ;; https://www.emacswiki.org/emacs/SavePlace
  (save-place-mode 1)

  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "M-z") 'zap-up-to-char)

  (show-paren-mode 1)
  (electric-pair-mode 1)
  (setq-default indent-tabs-mode nil)
  (savehist-mode 1)
  (setq
   history-delete-duplicates t
   savehist-save-minibuffer-history t
   savehist-additional-variables '(kill-ring
                                   search-ring
                                   regexp-search-ring))
  (setq save-interprogram-paste-before-kill t
        apropos-do-all t
        mouse-yank-at-point t
        require-final-newline t
        visible-bell t
        load-prefer-newer t
        backup-by-copying t
        frame-inhibit-implied-resize t
        ediff-window-setup-function 'ediff-setup-windows-plain
        custom-file (expand-file-name "custom.el" user-emacs-directory))

  (unless backup-directory-alist
    (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                   "backups")))))
  (column-number-mode 1)
  (mouse-avoidance-mode 'exile))

(server-start)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(defgroup proxy-settings nil
  "Set proxy address"
  :group 'Communication)

(defcustom http-proxy nil
  "HTTP/HTTPS proxy's address and port"
  :type 'string
  :group 'proxy-settings)

(defcustom socks-proxy nil
  "Socks proxy's address and port"
  :type 'string
  :group 'proxy-settings)

(use-package smex
  :config
  (smex-initialize)
  :bind
  ("M-x" . smex))

(use-package rg
  :bind
  ("C-c s" . rg)
  :config
  (add-to-list 'rg-custom-type-aliases '("chp" . "*.[chH] *.[ch]pp *.cc *.C")))

(use-package yasnippet
  :diminish yas-minor-mode
  :commands (yas-expand)
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all nil))

(setq completion-ignore-case t)

(defun custom/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun custom/backward-delete-word (arg)
  "Delete characters backward until encountering the begnning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (custom/delete-word (- arg)))

(global-set-key (kbd "M-DEL") 'custom/backward-delete-word)

(setq text-scale-mode-step 1.1)

(defun indent-region-advice (&rest ignored)
  (let ((deactivate deactivate-mark))
    (if (region-active-p)
        (indent-region (region-beginning) (region-end))
      (indent-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark deactivate)))

(use-package move-text
  :bind
  (("M-p" . move-text-up)
   ("M-n" . move-text-down))
  :config
  (advice-add 'move-text-up :after 'indent-region-advice)
  (advice-add 'move-text-down :after 'indent-region-advice))

(use-package recentf
  :config
  (recentf-mode 1)
  (setq recentf-auto-cleanup 'never)
  :bind ("C-x C-r" . counsel-recentf))

(defun custom/eglot-managed-mode-initialize ()
  (setq-local
   eldoc-documentation-functions
   (list
    #'eglot-signature-eldoc-function
    ;; #'eglot-hover-eldoc-function
    ;; #'flymake-eldoc-function
    )))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) .
                                        ("clangd"
                                         "-j=8"
                                         "--log=error"
                                         "--malloc-trim"
                                         "--background-index"
                                         "--clang-tidy"
                                         "--completion-style=detailed"
                                         "--pch-storage=memory"
                                         "--header-insertion=never"
                                         "--header-insertion-decorators=0")))
  ;; (add-to-list 'eglot-stay-out-of 'eldoc)
  (setq eglot-report-progress nil)
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (setq eglot-events-buffer-size 0)
  :hook
  ((c++-mode c-mode) . eglot-ensure)
  (eglot-managed-mode . custom/eglot-managed-mode-initialize))

(use-package eldoc
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  (eldoc-add-command 'c-electric-paren))

(use-package counsel
  :bind
  (("C-c l" . counsel-list-processes)
   ("C-c m" . counsel-semantic-or-imenu)
   ("C-h f" . counsel-describe-function)
   ("C-h v" . counsel-describe-variable)
   ("C-h b" . counsel-descbinds)
   ("C-h a" . counsel-apropos)))

(use-package company
  :hook
  (after-init . global-company-mode)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection)
        ("C-w" . nil)
        ("RET" . nil)
        ("<return>" . nil)))

(use-package isearch
  :config
  (setq isearch-lazy-count t
        lazy-count-prefix-format "%s/%s "
        isearch-lazy-highlight 'all-windows
        lazy-highlight-buffer t
        lazy-highlight-cleanup nil
        lazy-highlight-max-at-a-time nil
        lazy-highlight-initial-delay 0)
  :bind
  (("C-." . isearch-forward-symbol-at-point)
   ("C-c n" . lazy-highlight-cleanup)))

(defun custom/uppercase-previous-symbol ()
  "Used in insert-mode and change the previous symbol (or word in Vim lang)
to uppercase"
  (interactive)
  (let (beg end)
    (when (eq evil-state 'insert)
      (forward-symbol -1)
      (setq beg (point))
      (forward-symbol 1)
      (setq end (point))
      (evil-upcase beg end))))

(use-package evil
  :init
  (evil-mode t)
  (setq evil-symbol-word-search t)
  (defalias 'forward-evil-word 'forward-evil-symbol)
  :config
  (define-key evil-insert-state-map (kbd "M-u") #'custom/uppercase-previous-symbol))

(use-package smerge-mode
  :bind
  (:repeat-map smerge-repeat-map
               ("n" . smerge-next)
               ("p" . smerge-prev)
               ("u" . smerge-keep-upper)
               ("l" . smerge-keep-lower)))

(use-package topsy
  :hook
  (prog-mode . topsy-mode)
  (magit-section-mode . topsy-mode))

;;; programming languages
;; C-like
(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (other . "stroustrup")))

(load custom-file)
(load (expand-file-name "text-manipulate.el" user-emacs-directory))

(use-package display-line-numbers
  :if (>= emacs-major-version 26)
  :config
  (global-display-line-numbers-mode t)
  (set-face-background 'line-number-current-line "yellow")
  (set-face-foreground 'line-number-current-line "black"))

(use-package whitespace
  :diminish global-whitespace-mode
  :config
  (progn
    (global-whitespace-mode 1)
    (setq whitespace-style (quote (face tabs trailing tab-mark)))
    (setq whitespace-display-mappings
          ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
          '(
            (tab-mark 9 [10155 9] [92 9])
            ))
    (if (display-graphic-p)
        (progn
          (set-face-foreground 'whitespace-newline "#eddfba")
          (set-face-inverse-video 'whitespace-tab nil))
      (progn
        (set-face-foreground 'whitespace-newline "#cd00cd")
        (set-face-foreground 'whitespace-tab "#cd00cd")
        (set-face-background 'whitespace-tab 'unspecified)))))

;;; init.el ends here
