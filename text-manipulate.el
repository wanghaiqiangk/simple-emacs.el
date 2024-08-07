;;; text-manipulate.el --- Inspired by other tools

;;; Commentary:
;; Functions and features to ease text manipulation which are inspired by
;; other great tools, especially like vim.

;;; Code:

(defun change-inner (prefix character)
  "Kill region inside delimiters, using either beginning or
ending delimiter.  With prefix arg, kill including delimiters."

  (interactive "p\ncpaired-char: ")
  (let ((initial-point (point))
        (start)
        (end)
        (move-point-by (if (> prefix 1) 0 1)))

    (condition-case nil
        (progn
          ;; Search forward for given char
          (search-forward (char-to-string character))
          (setq end (- (point) move-point-by))

          (condition-case nil
              (backward-sexp)
            (error (backward-list)))

          (setq start (+ (point) move-point-by))
          (kill-region start end)
          (or (= prefix 4) (forward-char)))

      (error (progn
               ;; Reset and search backward for given char
               (goto-char initial-point)
               (search-backward (char-to-string character))
               (setq start (+ (point) move-point-by))

               (condition-case nil
                   (forward-list)
                 (error (forward-sexp))))

             (setq end (- (point) move-point-by))
             (kill-region start end)))))

(defun change-outer ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'change-inner)))

(global-set-key (kbd "C-c i") 'change-inner)
(global-set-key (kbd "C-c a") 'change-outer)

(provide 'text-manipulate)

;;; text-mainpulate.el ends here
