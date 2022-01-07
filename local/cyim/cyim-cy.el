;;; cyim-cy.el

;;; Features:
;; 1. 能导入输入历史
;; 2. 提供造词的命令
;; 3. 提供候选的单字
;; 4. 拼音输入
;; 5. 处理标点
;; 6. 使用 [ ' 快速选择

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'cyim-table)

(defgroup cyim-cy nil
  "cyim input method"
  :group 'cyim)
  
(defcustom cyim-cy-history-file "~/.emacs.d/cyim-history"
  "保存选择的历史记录."
  :type 'file
  :group 'cyim-cy)

(defcustom cyim-cy-user-file "~/.emacs.d/mycy.txt"
  "保存用户自造词."
  :type 'file
  :group 'cyim-cy)

(defcustom cyim-cy-save-always nil
  "是否每次加入新词都要保存.
当然设置为 nil，也会在退出 Emacs 里保存一下的."
  :type 'boolean
  :group 'cyim-cy)

(defcustom cyim-cy-add-all-completion-limit 3
  "在超过输入字符串超过这个长度时会添加所有补全."
  :type 'integer
  :group 'cyim-cy)

(defvar cyim-cy-load-hook nil)
(defvar cyim-cy-package nil)
(defvar cyim-cy-char-table (make-vector 1511 0))
(defvar cyim-cy-punctuation-list nil)
(defvar cyim-cy-initialized nil)

(defun cyim-cy-create-word (word)
  "Insert WORD to database and write into user file."
  (let ((len (length word))
        code)
    (setq code
     (cond
      ((= len 2)
       (concat (substring (cyim-table-get-char-code (aref word 0)) 0 2)
               (substring (cyim-table-get-char-code (aref word 1)) 0 2)))
      ((= len 3)
       (concat (substring (cyim-table-get-char-code (aref word 0)) 0 1)
               (substring (cyim-table-get-char-code (aref word 1)) 0 1)
               (substring (cyim-table-get-char-code (aref word 2)) 0 2)))
      (t
       (concat (substring (cyim-table-get-char-code (aref word 0)) 0 1)
               (substring (cyim-table-get-char-code (aref word 1)) 0 1)
               (substring (cyim-table-get-char-code (aref word 2)) 0 1)
               (substring (cyim-table-get-char-code (aref word (1- (length word)))) 0 1)))))))

;;;_. load it
(unless cyim-cy-initialized
  (setq cyim-cy-package cyim-current-package)
  (setq cyim-cy-punctuation-list
        (cyim-read-punctuation cyim-cy-package))
  (let ((map (cyim-mode-map)))
    (define-key map "\t" 'cyim-table-show-completion)
    (define-key map "[" 'cyim-quick-select-1)     ;; cy 中用到了 ;
    ;(define-key map "'" 'cyim-quick-select-2)
    )
  (defvar cyim-cy-use-gbk nil)
  (let ((path (file-name-directory load-file-name)))
    (load (concat path
                  (if (and (boundp 'cyim-cy-use-gbk)
                           cyim-cy-use-gbk)
                      "cyim-cy-gbk" "cyim-cy-gb2312"))))

  (cyim-table-add-user-file cyim-cy-user-file)
  (cyim-table-load-history cyim-cy-history-file)
  (run-hooks 'cyim-cy-load-hook)
  (cyim-set-option 'table-create-word-function 'cyim-cy-create-word)
  (cyim-set-option 'punctuation-list 'cyim-cy-punctuation-list)
  (cyim-set-option 'max-length 4)
  (cyim-set-option 'translate-chars '(?z))
  (cyim-set-option 'all-completion-limit cyim-cy-add-all-completion-limit)
  (cyim-set-option 'char-table cyim-cy-char-table)
  (cyim-set-active-function 'cyim-table-active-function)
  (setq cyim-cy-initialized t))

(provide 'cyim-cy)
;;; cyim-cy.el ends here
