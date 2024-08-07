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

(electric-pair-mode 1)

(require 'package)

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

(defcustom package-archive-alist
  (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		      (not (gnutls-available-p))))
	 (proto (if no-ssl "http" "https")))
    (when no-ssl
      (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
    `((official . (("gnu"    . ,(format "%s://elpa.gnu.org/packages/" proto))
		   ("melpa"  . ,(format "%s://melpa.org/packages/" proto))
		   ("nongnu" . ,(format "%s://elpa.nongnu.org/nongnu/" proto))))
      (emacs-china . (("gnu"    . "http://1.15.88.122/gnu/")
		      ("melpa"  . "http://1.15.88.122/melpa/")
		      ("nongnu" . "http://1.15.88.122/nongnu/")))
      (tuna . (("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
	       ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
	       ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")))
      (ustc . (("gnu"    . "https://mirrors.ustc.edu.cn/elpa/gnu/")
	       ("melpa"  . "https://mirrors.ustc.edu.cn/elpa/melpa/")
	       ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))))
  "A list of package archives."
  :group 'Convenience
  :type '(alist :key-type (symbol :tag "Archive group")
		:value-type (alist :key-type (string :tag "Archive")
				   :value-type (string :tag "URL"))))

(defcustom package-archive-in-effect 'official
  "Set package archive."
  :group 'Convenience
  :set (lambda (symbol value)
	 (set symbol value)
	 (setq package-archives
	       (or (alist-get value package-archive-alist)
		   (error "Unknown package archive: `%s'" value))))
  :type `(choice ,@(mapcar (lambda (item)
			     (let ((archive (car item)))
			       (list 'const :tag (capitalize (symbol-name archive)) archive)))
			   package-archive-alist)))

;;; init.el ends here
