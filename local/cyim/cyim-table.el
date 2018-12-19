;;; cyim-table.el


;;; Commentary:

;; - punctuation-list: A symbol to translate punctuation
;; - translate-chars: The first letter which will invoke reverse
;;                   search the code for char
;; - max-length: max input string length
;; - char-table: a obarray to search code for char
;; - all-completion-limit: A minimal length to add all completions
;; - table-create-word-function
;; 
;; - table-user-file
;; - table-history-file

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'cyim-table)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'cyim)
(require 'cyim-extra)

(defun cyim-table-translate (char)
  (cyim-punc-translate (symbol-value (cyim-get-option 'punctuation-list))
                      char))

(defun cyim-table-get-char-code (char)
  (cyim-get-char-code char (cyim-get-option 'char-table)))

(defun cyim-table-format (key cp tp choice)
  (if (memq (aref key 0) (cyim-get-option 'translate-chars))
      (setq choice
            (mapcar (lambda (c)
                      (if (consp c)
                          (setq c (car c)))
                      (cons c
                            (cyim-table-get-char-code (aref c 0))))
                    choice)))
  (let ((i 0))
    (format "%s[%d/%d]: %s"
            key  cp tp
            (mapconcat 'identity
                       (mapcar
                        (lambda (c)
                          (format "%d.%s " (setq i (1+ i))
                                  (if (consp c)
                                      (concat (car c) " " (cdr c))
                                    c)))
                        choice) " "))))

;;;_. 增加补全
(defun cyim-table-add-completion ()
  (if (= (length cyim-current-key) 1)
      t
    (let ((reg (concat "^" (regexp-quote cyim-current-key)))
          (len (length cyim-current-key))
          (package cyim-current-package)
          (key cyim-current-key)
          line completion)
      (save-excursion
        (dolist (buf (mapcar 'cdar (cyim-buffer-list)))
          (set-buffer buf)
          (setq cyim-current-package package)
          (beginning-of-line)
          (if (or (string= (cyim-code-at-point) key)
                  (not (looking-at reg)))
              (forward-line 1))
          (while (looking-at reg)
            (setq line (cyim-line-content))
            (mapc (lambda (c)
                    (when (or (>= len (cyim-get-option 'all-completion-limit))
                              (= (length c) 1))
                      (push (cons c (substring
                                     (car line)
                                     len))
                            completion)))
                  (cdr line))
            (forward-line 1))))
      (setq completion (sort (delete-dups (nreverse completion))
                             (lambda (a b)
                               (< (length (cdr a)) (length (cdr b))))))
      ;;      (message "%s, %s" cyim-current-choices completion)
      (setcar cyim-current-choices (append (car cyim-current-choices)
                                          completion))
      ;;      (message "%s, %s" cyim-current-choices completion))
      t)))

(defun cyim-table-stop-function ()
  (if (memq (aref cyim-current-key 0) (cyim-get-option 'translate-chars))
      nil
    (> (length cyim-current-key) (cyim-get-option 'max-length))))

(defun cyim-table-active-function ()
  (setq cyim-add-completion-function 'cyim-table-add-completion
        cyim-translate-function 'cyim-table-translate
        cyim-format-function 'cyim-table-format
        cyim-stop-function 'cyim-table-stop-function))

;; user file and history file
;;;_. cyim-cy-add-user-file
(defun cyim-table-add-user-file (file)
  (when file
    (let* ((buflist (cyim-buffer-list))
           (ufile (expand-file-name file))
           user-buffer)
      (or (file-exists-p ufile)
          (setq ufile (locate-file file load-path)))
      (when (and ufile (file-exists-p ufile))
        ;; make sure the file not load again
        (mapc (lambda (buf)
                (if (string= (expand-file-name (cdr (assoc "file" buf)))
                             ufile)
                    (setq user-buffer (cdr (assoc "buffer" buf)))))
              buflist)
        (unless user-buffer
          (setq file (cyim-read-file ufile (format cyim-buffer-name-format
                                                  (cyim-package-name))))
          (cyim-make-char-table (cyim-table-get-user-char (cdar file)) (cyim-get-option 'char-table))
          (nconc buflist (list file))
          (cyim-set-option 'table-user-file (cons ufile (cdar file))))))))

(defun cyim-table-get-user-char (buf)
  "Add user characters. Currently cyim-cy may not contain all
chinese characters, so if you want more characters to input, you
can add here."
  (let (line chars)
    (save-excursion
      (set-buffer buf)
      (goto-char (point-min))
      (while (not (eobp))
        (setq line (cyim-line-content))
        (forward-line 1)
        (if (and (= (length (cadr line)) 1)
                 (> (length (car line)) 2))
            (push line chars)))
      chars)))

(defun cyim-table-load-history (his-file)
  (when (and his-file (file-exists-p his-file))
    (ignore-errors
      (cyim-load-history his-file cyim-current-package)
      (cyim-set-option 'record-position t)
      (cyim-set-option 'table-history-file his-file))))

(defun cyim-table-save-history ()
  "Save history and user files."
  (dolist (package cyim-package-list)
    (let* ((cyim-current-package (cdr package))
           (his-file (cyim-get-option 'table-history-file))
           (user-file (cyim-get-option 'table-user-file)))
      (when (and his-file
                 (file-exists-p his-file)
                 (file-writable-p his-file))
        (cyim-save-history his-file cyim-current-package))
      (when (and user-file
                 (file-exists-p (car user-file))
                 (file-writable-p (car user-file)))
        (with-current-buffer (cdr user-file)
          (save-restriction
            (widen)
            (write-region (point-min) (point-max) (car user-file))))))))
;; 按 TAB 显示补全
(defun cyim-table-show-completion ()
  (interactive)
  (if (eq last-command 'cyim-table-show-completion)
      (ignore-errors
        (with-selected-window (get-buffer-window "*Completions*")
          (scroll-up)))
    (if (or (= (length cyim-current-key) 1) (= (aref cyim-current-key 0) ?z))
        nil
      (while (not (cyim-add-completion)))
      (let ((choices (car cyim-current-choices))
            completion)
        (dolist (c choices)
          (if (listp c)
              (push (list (format "%-4s %s"
                                  (concat cyim-current-key (cdr c))
                                  (car c)))
                    completion)))
        (with-output-to-temp-buffer "*Completions*"
          (display-completion-list
           (all-completions cyim-current-key (nreverse completion))
           cyim-current-key)))))
  (funcall cyim-handle-function))

;; 增加新词
(defvar cyim-table-minibuffer-map nil)
(defvar cyim-table-save-always nil)
(when (null cyim-table-minibuffer-map)
  (setq cyim-table-minibuffer-map
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map minibuffer-local-map)
          (define-key map "\C-e" 'cyim-table-minibuffer-forward-char)
          (define-key map "\C-a" 'cyim-table-minibuffer-backward-char)
          map)))
;;;_. 增加新词
(defun cyim-table-minibuffer-forward-char ()
  (interactive)
  (end-of-line)
  (let ((char (save-excursion
                (set-buffer buffer)
                (char-after end))))
    (when char
      (insert char)
      (incf end))))

(defun cyim-table-minibuffer-backward-char ()
  (interactive)
  (beginning-of-line)
  (let ((char (save-excursion
                (set-buffer buffer)
                (when (>= start (point-min))
                  (decf start)
                  (char-after start)))))
    (when char
      (insert char))))

(defun cyim-table-add-word ()
  "Create a map for word. The default word is the two characters
before cursor. You can use C-a and C-e to add character at the
begining or end of the word.

默认新词为光标前的两个字，通过两个按键延长这个词：
 C-e 在头部加入一个字
 C-a 在尾部加入一个字
"
  (interactive)
  (let* ((buffer (current-buffer))
         (end (point))
         (start (- (point) 2))
         (word (buffer-substring-no-properties
                start end))
         (user-file (cyim-get-option 'table-user-file))
         (func (cyim-get-option 'table-create-word-function))
         choice code words)
    (when func
      (setq word (read-from-minibuffer "加入新词: " word
                                       cyim-table-minibuffer-map)
            code (funcall func word))
      (setq choice (cyim-get code))
      (unless (member word (car choice))
        (if (buffer-live-p (cdr user-file))
            (save-excursion
              (set-buffer (cdr user-file))
              (if (string-match "^\\s-$" (buffer-string))
                  (insert "\n" code " " word)
                (cyim-bisearch-word code (point-min) (point-max))
                (let ((words (cyim-line-content)))
                  (goto-char (line-end-position))
                  (if (string= (car words) code)
                      (insert " " word)
                    (insert "\n" code " " word))))
              (setcar choice (append (car choice) (list word)))
              (if cyim-table-save-always
                  (save-restriction
                    (widen)
                    (write-region (point-min) (point-max) (car user-file)))))
          (error "the user buffer is closed!")))))
  (message nil))

(add-hook 'kill-emacs-hook 'cyim-table-save-history)

(provide 'cyim-table)
;;; cyim-table.el ends here
