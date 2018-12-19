;;; cyim-extra.el

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'cyim)

(defvar cyim-punc-escape-list
  (number-sequence ?0 ?9)
  "Punctuation will not insert after this characters.
If you don't like this funciton, set the variable to nil")
(defvar cyim-insert-ascii-char (cons ?\; "；")
  "*Key used for `cyim-insert-ascii'.")

(defvar cyim-punc-translate-p t
  "*Non-nil means will translate punctuation.")

;;;_. handle punctuation
(defun cyim-read-punctuation (package)
  (let ((cyim-current-package package)
	      buf punc-list punc)
    (setq buf (cdr (assoc "buffer" (car (cyim-buffer-list)))))
    (save-excursion
      (set-buffer buf)
      (save-restriction
        (widen)
        (let ((region (cyim-section-region "Punctuation")))
          (goto-char (car region))
          (while (< (point) (cdr region))
            (setq punc (cyim-line-content))
            (if (> (length punc) 3)
                (error "标点不支持多个转换！"))
            (add-to-list 'punc-list punc)
            (forward-line 1)))))
    punc-list))

(defun cyim-punc-translate (punc-list char)
  (if cyim-punc-translate-p
      (cond ((< char ? ) "")
            ((and cyim-insert-ascii-char
                  (= char (car cyim-insert-ascii-char)))
             (char-to-string char))
            (t (let ((str (char-to-string char))
                     punc)
                 (if (and (not (member (char-before) cyim-punc-escape-list))
                          (setq punc (cdr (assoc str punc-list))))
                     (progn
                       (if (= char (char-before))
                           (delete-char -1))
                       (if (= (safe-length punc) 1)
                           (car punc)
                         (setcdr (cdr punc) (not (cddr punc)))
                         (if (cddr punc)
                             (car punc)
                           (nth 1 punc))))
                   str))))
    (char-to-string char)))

(defun cyim-punc-translate-toggle (arg)
  (interactive "P")
  (setq cyim-punc-translate-p
        (if (null arg)
            (not cyim-punc-translate-p)
          (> (prefix-numeric-value arg) 0))))

;;;_. 一个快速插入英文的命令。按自己的需要绑定
(defun cyim-insert-ascii ()
  (interactive)
  (if current-input-method
      (let (c)
        (message "英文：")
        (setq c (read-event))
        (cond ((= c ? ) (insert (cdr cyim-insert-ascii-char)))
              ((= c ?\r) (insert-char (car cyim-insert-ascii-char) 1))
              (t
               (setq unread-command-events (list last-input-event))
               (insert (read-from-minibuffer "英文：")))))
    (call-interactively 'self-insert-command)))

;; 切换到英文状态时，先显示第一个字母
(defun cyim-insert-ascii-first ()
  (interactive)
  (if current-input-method
      (let (c)
	      (message (concat "英文：" (char-to-string last-input-event)))
	      (insert (char-to-string last-input-event))
        (setq c (read-event))
        (cond ((= c ? ) (insert (cdr cyim-insert-ascii-char)))
              ((= c ?\r) (insert-char (car cyim-insert-ascii-char) 1))
              (t
               (setq unread-command-events (list last-input-event))
               (insert (read-from-minibuffer (concat "英文：" (char-to-string last-command-event)))))))
    (call-interactively 'self-insert-command)))

;; 切换输入法
(defun cyim-toggle ()
  (interactive)
  (if current-input-method
      (progn
	      (insert (char-to-string last-input-event))
        (call-interactively 'toggle-input-method))
    (call-interactively 'self-insert-command)))

;; Evil 中，在 normal 状态下关闭输入法
(defun cyim-evil-normal-toggle ()
  (interactive)
  (call-interactively 'evil-insert)
  (if current-input-method
      (progn
        (call-interactively 'toggle-input-method)))
  (call-interactively 'evil-force-normal-state))

;; 取消当前输入，并切换到英文
(defun cyim-clear-toggle ()
  (interactive)
  (cyim-quit-clear)
  (when current-input-method
    (call-interactively 'toggle-input-method)))

;;;_. load and save history
(defun cyim-load-history (history-file package)
  (let* ((cyim-current-package package)
         (history (cyim-history))
         item)
    (when (file-exists-p history-file)
      (with-current-buffer (find-file-noselect history-file)
        (goto-char (point-min))
        (while (not (eobp))
          (if (and (setq item (cyim-line-content))
                   (= (length item) 2))
              (puthash (car item)
                       `(nil ("pos" . ,(string-to-number (cadr item))))
                       history))
          (forward-line 1))
        (kill-buffer (current-buffer))))))

(defun cyim-save-history (history-file package)
  (interactive)
  (let* ((cyim-current-package package)
         (history (cyim-history)))
    (with-temp-buffer
      (erase-buffer)
      (let (pos)
        (maphash (lambda (key val)
                   (unless (or (cyim-string-emptyp key)
                               (= (setq pos (cdr (assoc "pos" (cdr val)))) 1))
                     (insert key " " (number-to-string pos) "\n")))
                 history))
      (write-file history-file))))

;;;_. 增加两个快速选择的按键
(defun cyim-quick-select-1 ()
  "如果没有可选项，插入数字，否则选择对应的词条."
  (interactive)
  (if (car cyim-current-choices)
      (let ((index (cyim-page-start))
            (end (cyim-page-end)))
        (if (>= index end)
            (cyim-append-string (cyim-translate last-command-event))
          (cyim-remember-select (1+ index))
          (setq cyim-current-str (cyim-choice (nth index (car cyim-current-choices))))))
    (cyim-append-string (cyim-translate last-command-event)))
  (cyim-terminate-translation))

(defun cyim-quick-select-2 ()
  "如果没有可选项，插入数字，否则选择对应的词条."
  (interactive)
  (if (car cyim-current-choices)
      (let ((index (1+ (cyim-page-start)))
            (end (cyim-page-end)))
        (if (>= index end)
            (cyim-append-string (cyim-translate last-command-event))
          (cyim-remember-select (1+ index))
          (setq cyim-current-str (cyim-choice (nth index (car cyim-current-choices))))))
    (cyim-append-string (cyim-translate last-command-event)))
  (cyim-terminate-translation))

(defun cyim-describe-char (pos package)
  (interactive
   (list (point)
         (if (eq input-method-function 'cyim-input-method)
             (cyim-package-name)
           (let (cyim-current-package)
             (setq cyim-current-package
                   (if (= (length cyim-package-list) 1)
                       (cdar cyim-package-list)
                     (assoc
                      (completing-read "In package: "
                                       cyim-package-list nil t
                                       (caar cyim-package-list))
                      cyim-package-list)))
             (cyim-package-name)))))
  (if (>= pos (point-max))
      (error "No character follows specified position"))
  (let ((char (char-after pos))
        (func (intern-soft (format "%s-get-char-code" package)))
        code)
    (when func
      (setq code (funcall func char))
      (if code
          (message "Type %S to input %c for input method %s"
                   code char package)
        (message "Can't find char code for %c" char)))))

;;;_. char table
(defun cyim-make-char-table (chars table)
  "Set CHARS of `cyim-char-database' in TABLE."
  (dolist (char chars)
    (let ((code (car char)))
      (dolist (c (cdr char))
        (set (intern c table) code)))))

(defsubst cyim-get-char-code (char table)
  "Get the code of the character CHAR in TABLE."
  (symbol-value (intern-soft (char-to-string char) table)))

(provide 'cyim-extra)
;;; cyim-extra.el ends here
