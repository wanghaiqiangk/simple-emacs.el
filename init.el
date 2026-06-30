;;; init.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Code:

(require 'package)
(setq package-archives '(("gnu"    . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

(require 'cl-lib)

(defvar my-packages
  '(use-package
     smex
     company
     magit
     magit-ido
     diminish
     rg
     yasnippet
     yasnippet-snippets
     move-text
     swiper
     ivy
     counsel
     ivy-hydra
     solarized-theme
     evil
     goto-chg
     topsy
     racket-mode
     company-box
     fzf
     ;; helm
     ido-completing-read+
     clang-format
     )
  "A list of packages to ensure are installed at launch.")

(defun my-packages-installed-p ()
  (cl-loop for p in my-packages
           when (not (package-installed-p p)) do (cl-return nil)
           finally (cl-return t)))

(unless (my-packages-installed-p)
  ;; check for new packages (package versions)
  (package-refresh-contents)
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

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
  (mouse-avoidance-mode 'exile)
  (setq compilation-skip-threshold 2)
  (setq visible-bell nil))

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
                                         "--header-insertion-decorators")))
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

(use-package company-box
  :diminish company-box-mode
  :hook (company-mode . company-box-mode))

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
  (evil-mode -1)
  (setq evil-symbol-word-search t)
  (defalias 'forward-evil-word 'forward-evil-symbol)
  :config
  (define-key evil-insert-state-map (kbd "M-u") #'custom/uppercase-previous-symbol)
  (evil-set-leader nil (kbd "SPC"))
  (define-key evil-normal-state-map (kbd "<leader>ff") 'ido-find-file)
  (define-key evil-normal-state-map (kbd "<leader>fr") 'counsel-recentf)
  (define-key evil-normal-state-map (kbd "<leader>bb") 'ido-switch-buffer)
  (define-key evil-normal-state-map (kbd "gcc") 'comment-line)
  (define-key evil-visual-state-map (kbd "gcc") 'comment-dwim)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-insert-state-map (kbd "C-u") 'evil-delete-back-to-indentation)
  (define-key evil-normal-state-map (kbd "<leader>/") 'rg))

(repeat-mode 1)
(defun repeatify (repeat-map)
  "Set the `repeat-map' property on all commands bound in REPEAT-MAP."
  (named-let process ((keymap (symbol-value repeat-map)))
    (map-keymap
     (lambda (_key cmd)
       (cond
        ((symbolp cmd) (put cmd 'repeat-map repeat-map))
        ((keymapp cmd) (process cmd))))
     keymap)))

(with-eval-after-load 'smerge-mode
  (repeatify 'smerge-basic-map))

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

(when (file-exists-p custom-file)
  (load custom-file))
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

(use-package magit
  :init
  (setq magit-define-global-key-bindings 'recommended)
  :config
  (setq magit-diff-refine-hunk t)
  (setq magit-diff-refine-ignore-whitespace t)
  (setq magit-ediff-dwim-show-on-hunks t)
  (setq magit-completing-read-function 'magit-ido-completing-read))

(with-eval-after-load 'magit
  (require 'magit-ido)
  (setq magit-completing-read-function
        'magit-ido-completing-read)
  ;; Optional:
  (keymap-set ido-common-completion-map
              "C-x g" 'magit-ido-enter-magit-status))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'toggle-window-split)

(load (expand-file-name "misc-cmds.el" user-emacs-directory))
(global-set-key (kbd "C-x k") 'kill-buffer-and-its-windows)
(global-set-key [remap quit-window] 'quit-window-delete)

;; (use-package fzf
;;   :bind
;;   (("C-x C-f" . (lambda ()
;;                   (interactive)
;;                   (let* ((pj (project-current t))
;;                          (root (project-root pj)))
;;                     (fzf-with-command "fd --hidden --follow --exclude \".git\" --exclude \".cache\"" #'fzf--action-find-file root))))))

(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(defun smerge-resolve-all-in-file-to (to-keep)
  "Resolves all conflicts inside a file in preference of TO-KEEP

TO-KEEP decides which part to keep and is one of `upper',
`lower', `base'"
  (interactive
   (list (completing-read "Keeping (upper, base, lower): "
                          '(upper base lower))))
  (let ((resolve-func
         (pcase to-keep
           ("upper" 'smerge-keep-upper)
           ("base"  'smerge-keep-base)
           ("lower" 'smerge-keep-lower)
           (_ (error "Unknown resolution argument!"))))
        (num-chars-bfore (point-max)))
    (save-excursion
      (goto-char (point-min))
      (while (ignore-errors (not (smerge-next)))
        (funcall resolve-func)))
    (when (= num-chars-bfore (point-max))
      (message "No conflicts were found"))))

(if (eq system-type 'darwin)
    (setq mac-command-modifier      'meta
          mac-option-modifier       'super
          mac-control-modifier      'control
          mac-function-modifier     'hyper))

;;; init.el ends here
