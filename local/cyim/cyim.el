;;; cyim.el

;;; Commentary:

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'help-mode)

(defvar cyim-version "1.0")

;;;_. emacs21 compatible
(when (not (fboundp 'number-sequence))
  (defun number-sequence (from &optional to inc)
    (if (and to (<= from to))
        (cons from
              (number-sequence (+ from (or inc 1)) to inc)))))

(when (not (fboundp 'delete-dups))
  (defun delete-dups (list)
    "Destructively remove `equal' duplicates from LIST.
Store the result in LIST and return it.  LIST must be a proper list.
Of several `equal' occurrences of an element in LIST, the first
one is kept."
    (let ((tail list))
      (while tail
        (setcdr tail (delete (car tail) (cdr tail)))
        (setq tail (cdr tail))))
    list))

;;;_. customize varable
(defgroup cyim nil
  "cyim: emacs input method"
  :group 'lcyim)
(defvar cyim-page-length 7 "每页显示的词条数目")

(defface cyim-string-face '((t (:underline t)))
  "Face to show current string"
  :group 'cyim)

;;;_. variable declare
(defvar cyim-package-list nil "所有正在使用的输入法")
(defvar cyim-current-package (make-vector 5 nil)
  "当前使用的输入法，一个 vector，有五个部分: package-name,
buffer-list,history, keymap, active-function.

buffer-list 中的每个 buffer 是这样的一个 Association List：
----------------------------------------
buffer         对应的 buffer
param          Parameter 部分的参数
file           对应的文件名
")
(defvar cyim-first-char (number-sequence ?a ?z) "Table 中所有首字母列表")
(defvar cyim-total-char (number-sequence ?a ?z) "所有可能的字符")
(defvar cyim-do-completion t "是否读入可能的补全")

(defvar cyim-current-key "" "已经输入的代码")
(defvar cyim-current-str "" "当前选择的词条")
(defvar cyim-current-choices nil "所有可选的词条。

这个 list 的 CAR 是可选的词条，一般是一个字符串列表，但是也可以含有
list。但是这个 list 的第一个元素必须是将要插入的字符串。

CDR 部分是一个 Association list。通常含有这样的内容：
---------------------------
pos         上次选择的位置
completion  下一个可能的字母（如果 cyim-do-completion 为 t）
")
(defvar cyim-current-pos nil "当前选择的词条在 cyim-current-choices 中的位置")
(defvar cyim-guidance-str "" "显示可选词条的字符串")
(defvar cyim-translating nil "记录是否在转换状态")
(defvar cyim-overlay nil "显示当前选择词条的 overlay")
(defvar cyim-guidance-frame nil)
(defvar cyim-guidance-buf nil)

(defvar cyim-load-hook nil)
(defvar cyim-active-hook nil)

(defvar cyim-stop-function nil)
(defvar cyim-translate-function nil)
(defvar cyim-add-completion-function nil)
(defvar cyim-format-function 'cyim-format)
(defvar cyim-handle-function 'cyim-handle-string)

(defvar cyim-use-tooltip (not (or noninteractive
                                 emacs-basic-display
                                 (not (display-graphic-p))
                                 (not (fboundp 'x-show-tip)))))
(defvar cyim-tooltip-timeout 15)

(defvar cyim-buffer-name-format " *%s*"
  "buffer 的名字格式，%s 对应 package name")

(defvar cyim-quick-en t
  "默认打开快速英文切换功能")

(defvar cyim-show-first t
  "默认打开快速英文切换功能")

(defvar cyim-current-length 0
  "当前选择的词条字数")

(defvar cyim-mode-map          ; ## map
  (let ((map (make-sparse-keymap))
        (i ?\ ))
    (while (< i 127)
      (define-key map (char-to-string i) 'cyim-self-insert-command)
      (setq i (1+ i)))
    (setq i 128)
    (while (< i 256)
      (define-key map (vector i) 'cyim-self-insert-command)
      (setq i (1+ i)))
    (dolist (i (number-sequence ?1 ?9))
      (define-key map (char-to-string i) 'cyim-number-select))
    (dolist (i (number-sequence ?A ?Z))
      (define-key map (char-to-string i) 'cyim-toggle))
    (define-key map " " 'cyim-select-current)
    (define-key map [backspace] 'cyim-delete-last-char)
    (define-key map [delete] 'cyim-delete-last-char)
    (define-key map "\C-z" 'cyim-delete-last-char)
    (define-key map "\C-n" 'cyim-next-page)
    (define-key map "\C-p" 'cyim-previous-page)
    (define-key map "\C-m" 'cyim-quit-no-clear)
    (define-key map "\C-c" 'cyim-quit-clear)
    map)
  "Keymap")

(defvar cyim-local-variable-list
  '(cyim-current-package

    cyim-page-length
    cyim-first-char
    cyim-total-char
    cyim-do-completion

    cyim-current-key
    cyim-current-str
    cyim-current-choices
    cyim-current-pos
    cyim-guidance-str
    cyim-translating
    cyim-overlay
    cyim-guidance-frame
    cyim-guidance-buf

    cyim-load-hook
    cyim-active-hook

    cyim-translate-function
    cyim-format-function
    cyim-handle-function
    cyim-add-completion-function
    cyim-stop-function

    input-method-function
    inactivate-current-input-method-function
    describe-current-input-method-function)
  "A list of buffer local variable")

(dolist (var cyim-local-variable-list)
  (make-variable-buffer-local var)
  (put var 'permanent-local t))

;;;_ , package contents
(defsubst cyim-package-name ()
  (aref cyim-current-package 0))

(defsubst cyim-buffer-list ()
  (aref cyim-current-package 1))

(defsubst cyim-history ()
  "保存输入过的词的选择，另一方面加快搜索。另外在这里来处理标点。
这个散列中的每个元素都有这样的格式：
  ((list WORDS) other-properties)
OTHER-PROPERTIES 是一些其它的属性，比如，上次的位置，用来输入标点等。"
  (aref cyim-current-package 2))

(defsubst cyim-mode-map ()
  (aref cyim-current-package 3))

(defsubst cyim-options ()
  (aref cyim-current-package 4))

(defsubst cyim-active-function ()
  (aref cyim-current-package 5))

(defsubst cyim-set-package-name (name)
  (aset cyim-current-package 0 name))

(defsubst cyim-set-buffer-list (list)
  (aset cyim-current-package 1 list))

(defsubst cyim-set-history (history)
  (aset cyim-current-package 2 history))

(defsubst cyim-set-mode-map (map)
  (aset cyim-current-package 3 map))

(defsubst cyim-set-options (options)
  (aset cyim-current-package 4 options))

(defsubst cyim-set-active-function (func)
  (aset cyim-current-package 5 func))

(defun cyim-get-option (option)
  (cdr (assoc option (cyim-options))))
(defun cyim-set-option (option flag)
  (let ((options (cyim-options))
        opt)
    (if (setq opt (assoc option options))
        (setcdr opt flag)
      (push (cons option flag) options)
      (cyim-set-options options))))

;;;_. read file functions
(defun cyim-load-file (file)
  (let ((bufname (format cyim-buffer-name-format (cyim-package-name)))
        buflist buf param files)
    (save-excursion
      (setq buf (cyim-read-file file bufname t))
      (setq param (cdr (assoc "param" buf)))
      (setq buflist (append buflist (list buf)))
      (when (setq files (assoc "other-files" param))
        (setq files (split-string (cadr files) ";"))
        (dolist (f files)
          (if (file-exists-p (expand-file-name f))
              (setq f (expand-file-name f))
            (setq f (locate-file f load-path)))
          (setq buflist (append buflist (list (cyim-read-file f bufname))))))
      buflist)))

(defun cyim-read-file (file name &optional read-param)
  (let (param region)
    (save-excursion
      (set-buffer (generate-new-buffer name))
      (insert-file-contents file)
      (if read-param
          (setq param (cyim-read-parameters)))
      (setq region (cyim-section-region "Table"))
      (narrow-to-region (car region) (cdr region))
      `(("buffer" . ,(current-buffer))
        ("param" . ,param)
        ("file" . ,file)))))

(defun cyim-section-region (sec)
  "得到一个部分的起点和终点位置，忽略最后的空行"
  (let ((reg (concat "^\\[" sec "\\]\n")))
    (save-excursion
      (if (not (re-search-forward reg nil t))
          (if (re-search-backward reg nil t)
              (forward-line 1)
            (error "文件类型错误！没有 %s 部分！" sec)))
      (cons (point) (progn
                      (if (re-search-forward "^\\[\\sw+\\]\n" nil t)
                          (forward-line -1)
                        (goto-char (point-max)))
                      (re-search-backward "[^  \t\n]" nil t)
                      (1+ (point)))))))

(defun cyim-read-parameters ()
  "得到 [Parameter] 部分的参数，以 assoc list 的形式返回"
  (let* ((r (cyim-section-region "Parameter"))
         param pair)
    (goto-char (car r))
    (while (< (point) (cdr r))
      (when (setq pair (cyim-line-content "=" t))
        (add-to-list 'param pair))
      (forward-line 1))
    param))

;;;_. common functions

(defsubst cyim-delete-region ()
  "Delete the text in the current translation region of E+."
  (if (overlay-start cyim-overlay)
      (delete-region (overlay-start cyim-overlay)
                     (overlay-end cyim-overlay))))

;;; steal from emms-compat.el. Is this a good idea?
(when (not (fboundp 'emms-delete-if))
  (defun emms-delete-if (predicate seq)
    "Remove all items satisfying PREDICATE in SEQ.
This is a destructive function: it reuses the storage of SEQ
whenever possible."
    ;; remove from car
    (while (when (funcall predicate (car seq))
             (setq seq (cdr seq))))
    ;; remove from cdr
    (let ((ptr seq)
          (next (cdr seq)))
      (while next
        (when (funcall predicate (car next))
          (setcdr ptr (if (consp next)
                          (cdr next)
                        nil)))
        (setq ptr (cdr ptr))
        (setq next (cdr ptr))))
    seq))

(defun cyim-subseq (list from &optional to)
  (if (null to) (nthcdr from list)
    (butlast (nthcdr from list) (- (length list) to))))

(defun cyim-mod (x y)
  "like `mod', but when result is 0, return Y"
  (let ((base (mod x y)))
    (if (= base 0)
        y
      base)))

(defun cyim-string-emptyp (str)
  (not (string< "" str)))

(defun cyim-line-content (&optional seperaters omit-nulls)
  "用 SEPERATERS 分解当前行，所有参数传递给 split-string 函数"
  (let ((items   (split-string
                  (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position)) seperaters)))
    (if omit-nulls
        (emms-delete-if 'cyim-string-emptyp items)
      items)))

(defsubst cyim-delete-line ()
  (delete-region (line-beginning-position) (min (+ (line-end-position) 1)
                                                (point-max))))

(defsubst cyim-append-string (str)
  "append STR to cyim-current-str"
  (setq cyim-current-str (concat cyim-current-str str)))

;;;_. code search
(defun cyim-get (code)
  (when (and (stringp code) (not (cyim-string-emptyp code)))
    (let ((history (gethash code (cyim-history)))
          pos words completions)
      (if (and (car history) (assoc "completions" (cdr history)))
          history
        (dolist (buf (cyim-buffer-list))
          (with-current-buffer (cdr (assoc "buffer" buf))
            (setq words (append words
                                (cdr
                                 (cyim-bisearch-word code
                                                    (point-min)
                                                    (point-max)))))
            (if cyim-do-completion
                (setq completions (cyim-completions code completions)))))
        (setq words (delete-dups words))
        (puthash code (list words
                            (cons "pos" (or (cdr (assoc "pos" (cdr history))) 1))
                            (cons "completions" completions))
                 (cyim-history))))))

(defun cyim-completions (code completions)
  (let ((maxln 200)
        (cnt 0)
        (len (length code))
        (reg (concat "^" (regexp-quote code))))
    (save-excursion
      (forward-line 1)
      (while (and (looking-at reg)
                  (< cnt maxln))
        (add-to-list 'completions (buffer-substring-no-properties
                                   (+ (point) len)
                                   (+ (point) len 1)))
        (forward-line 1)
        (setq cnt (1+ cnt)))
      completions)))

(defun cyim-bisearch-word (code start end)
  (let ((mid (/ (+ start end) 2))
        ccode)
    (goto-char mid)
    (beginning-of-line)
    (setq ccode (cyim-code-at-point))
    ;;    (message "%d, %d, %d: %s" start mid end ccode)
    (if (string= ccode code)
        (cyim-line-content)
      (if (> mid start)
          (if (string< ccode code)
              (cyim-bisearch-word code mid end)
            (cyim-bisearch-word code start mid))))))

(defun cyim-code-at-point ()
  "Before calling this function, be sure that the point is at the
beginning of line"
  (save-excursion
    (if (re-search-forward "[ \t]" (line-end-position) t)
        (buffer-substring-no-properties (line-beginning-position) (1- (point)))
      (error "文件类型错误！%s 的第 %d 行没有词条！" (buffer-name) (line-number-at-pos)))))

;;;_. interface
(defun cyim-check-buffers ()
  "检查所有的 buffer 是否还存在，如果不存在，重新打开文件，如果文件不
存在，从 buffer-list 中删除这个 buffer"
  (let ((buflist (cyim-buffer-list))
        (bufname (cyim-package-name))
        buffer file)
    (dolist (buf buflist)
      (unless (buffer-live-p (cdr (setq buffer (assoc "buffer" buf))))
        (if (file-exists-p (setq file (cdr (assoc "file" buf))))
            (with-current-buffer (format "*%s*" (generate-new-buffer bufname))
              (insert-file-contents file)
              (setcdr buffer (current-buffer)))
          (message "%s for %s is not exists!" file bufname)
          (setq buflist (remove buf buflist)))))
    t))

(defun cyim-install-variable ()
  (let ((param (cdr (assoc "param" (car (cyim-buffer-list))))))
    (mapc (lambda (p)
            (let ((sym (intern-soft (concat "cyim-" (car p)))))
              (if sym
                  (set sym (mapconcat 'identity (cdr p) "=")))))
          param)
    (if (stringp cyim-page-length)
        (setq cyim-page-length (string-to-number cyim-page-length)))
    (setq cyim-first-char (append cyim-first-char nil)
          cyim-total-char (append cyim-total-char nil))))

;;;_ , cyim-use-package
(defun cyim-use-package (package-name &optional word-file active-func)
  (interactive)
  (mapc 'kill-local-variable cyim-local-variable-list)
  (mapc 'make-local-variable cyim-local-variable-list)
  (if (assoc package-name cyim-package-list)
      (setq cyim-current-package (cdr (assoc package-name
                                            cyim-package-list)))
    ;; make more room for extension
    (setq cyim-current-package (make-vector 9 nil)))
  (if (functionp active-func)
      (funcall active-func))
  (unless (and (cyim-package-name)
               (cyim-check-buffers))
    (if (and word-file
             (if (file-exists-p (expand-file-name word-file))
                 (setq word-file (expand-file-name word-file))
               (setq word-file (locate-file word-file load-path))))
        (progn
          (cyim-set-package-name package-name)
          (cyim-set-buffer-list (cyim-load-file word-file))
          (cyim-set-history (make-hash-table :test 'equal))
          (cyim-set-mode-map (let ((map (make-sparse-keymap)))
                              (set-keymap-parent map cyim-mode-map)
                              map))
          (add-to-list 'cyim-package-list (cons package-name cyim-current-package))
          (let ((param (cdr (assoc "param" (car (cyim-buffer-list))))))
            (if (assoc "lib" param)
                (load (cadr (assoc "lib" param)))))
          (run-hooks 'cyim-load-hook)
          (message nil))
      (error "没有这个文件: %s" word-file)))
  (cyim-install-variable)
  (setq input-method-function 'cyim-input-method)
  (setq inactivate-current-input-method-function 'cyim-inactivate)
  (setq describe-current-input-method-function 'cyim-help)
  ;; If we are in minibuffer, turn off the current input method
  ;; before exiting.
  (when (eq (selected-window) (minibuffer-window))
    (add-hook 'minibuffer-exit-hook 'cyim-exit-from-minibuffer))
  (run-hooks 'cyim-active-hook)
  (if (functionp (cyim-active-function))
      (funcall (cyim-active-function))))

(defun cyim-inactivate ()
  (interactive)
  (mapc 'kill-local-variable cyim-local-variable-list))

(defun cyim-help (&optional package)
  "Show input method docstring"
  (save-excursion
    (let ((map (cyim-mode-map))
          (elt (assoc (cyim-package-name) input-method-alist))
          reg desc docstr buf)
      (setq buf (cdr (assoc "buffer" (car (cyim-buffer-list)))))
      (set-buffer buf)
      (save-restriction
        (widen)
        (setq reg (condition-case err
                      (cyim-section-region "Description")
                    (error nil))
              desc (if reg
                       (buffer-substring-no-properties (car reg) (cdr reg))
                     "")
              docstr (format "Input method: %s (`%s' in mode line) for %s\n  %s\n%s\n\n%s\n"
                             (nth 0 elt) (nth 3 elt) (nth 1 elt) (nth 4 elt)
                             desc
                             (substitute-command-keys "\\{map}")))
        (help-setup-xref (list #'describe-input-method (nth 0 elt))
                         (interactive-p))
        (with-output-to-temp-buffer (help-buffer)
          (princ docstr))))))

;;;_ , page format
(defsubst cyim-choice (choice)
  (if (consp choice)
      (car choice)
    choice))

(defun cyim-add-completion ()
  "注意, cyim-add-completion-function 在没有完补全之前返回 nil, 在加完所
有补全之后一定要返回一个 t"
  (if (functionp cyim-add-completion-function)
      (funcall cyim-add-completion-function)
    t))

(defun cyim-format (key cp tp choice)
  (let ((i 0))
    (format "%s[%d/%d]: %s"
            key  cp tp
            (mapconcat 'identity
                       (mapcar
                        (lambda (c)
                          (format "%d.%s " (setq i (1+ i)) c))
                        choice) " "))))

(defun cyim-format-page ()
  "按当前位置，生成候选词条"
  (let ((end (cyim-page-end)))
    (if (car cyim-current-choices)
        (let* ((start (1- (cyim-page-start)))
               (choices (car cyim-current-choices))
               (choice (cyim-subseq choices start end))
               (pos (1- (min cyim-current-pos (length choices))))
               (i 0))
          (setq cyim-current-str (cyim-choice (nth pos choices)))
          (setq cyim-guidance-str
                (funcall cyim-format-function cyim-current-key (cyim-current-page)
                         (cyim-total-page) choice))
          ;; (message "%d, %s, %s" pos cyim-current-str cyim-guidance-str)
          (cyim-show))
      (setq cyim-current-str cyim-current-key)
      (setq cyim-guidance-str
            (concat cyim-current-key
                    (if (cdr (assoc "completions" (cdr cyim-current-choices)))
                        (format "[%s]: "
                                (mapconcat 'identity
                                           (cdr (assoc
                                                 "completions"
                                                 (cdr cyim-current-choices)))
                                           "")))))
      (cyim-show))))

(defun cyim-current-page ()
  (1+ (/ (1- cyim-current-pos) cyim-page-length)))

(defun cyim-total-page ()
  (1+ (/ (1- (length (car cyim-current-choices))) cyim-page-length)))

(defun cyim-page-start ()
  "计算当前所在页的第一个词条的位置"
  (let ((pos (min (length (car cyim-current-choices)) cyim-current-pos)))
    (1+ (- pos (cyim-mod pos cyim-page-length)))))

(defun cyim-page-end (&optional finish)
  "计算当前所在页的最后一个词条的位置，如果 cyim-current-choices 用
完，则检查是否有补全。如果 FINISH 为 non-nil，说明，补全已经用完了"
  (let* ((whole (length (car cyim-current-choices)))
         (len cyim-page-length)
         (pos cyim-current-pos)
         (last (+ (- pos (cyim-mod pos len)) len)))
    (if (< last whole)
        last
      (if finish
          whole
        (cyim-page-end (cyim-add-completion))))))

;;;_ , commands
(defun cyim-next-page (arg)
  (interactive "p")
  (if (> (length cyim-current-key) 0)
      (let ((new (+ cyim-current-pos (* cyim-page-length arg) 1)))
        (setq cyim-current-pos (if (> new 0) new 1)
              cyim-current-pos (cyim-page-start))
        (cyim-format-page))
    (message "%c" last-command-event)
    (cyim-append-string (cyim-translate last-command-event))
    (cyim-terminate-translation)))

(defun cyim-previous-page (arg)
  (interactive "p")
  (cyim-next-page (- arg)))

(defun cyim-delete-last-word ()
  (interactive)
  (delete-backward-char cyim-current-length))

(defun cyim-delete-last-char ()
  (interactive)
  (if (> (length cyim-current-key) 1)
      (progn
        (setq cyim-current-key (substring cyim-current-key 0 -1))
        (funcall cyim-handle-function))
    (setq cyim-current-str "")
    (cyim-terminate-translation)))

(defun cyim-self-insert-command ()
  "如果在 cyim-first-char 列表中，则查找相应的词条，否则停止转换，插入对应的字符"
  (interactive "*")
  ;; (message "%s" (current-buffer))
  (if (if (cyim-string-emptyp cyim-current-key)
          (member last-command-event cyim-first-char)
        (member last-command-event cyim-total-char))
      (progn
        (setq cyim-current-key (concat cyim-current-key (char-to-string last-command-event)))
        (funcall cyim-handle-function)

        ;; 如果输入了4个字母，且选项唯一，自动上第一个
        (when (= (length cyim-current-key) 4)
          (when (= 1 (length (car cyim-current-choices)))
            (cyim-terminate-translation))
          ;; 没有词是只显示key
          (when (string= cyim-current-str cyim-current-key)
            (setq cyim-current-str "")
            ;; (cyim-terminate-translation)
            )))
    (cyim-append-string (cyim-translate last-command-event))
    (cyim-terminate-translation)))

(defun cy-insert ()
  (interactive)
  (insert " "))

(defun cyim-select-current ()
  "如果没有可选项，而且是用空格来绑定这个键，就插入空格，否则选择第一
个词条"
  (interactive)
  (if (null (car cyim-current-choices))
      (setq cyim-current-str
            (if (> (length cyim-current-str) 0)
                ""
              (cyim-translate last-command-event)))
    (cyim-remember-select))
  (cyim-terminate-translation)

  ;; 输入空格时，自动切换到英文状态
  (when (and cyim-quick-en (string= cyim-current-str " "))
    (call-interactively 'toggle-input-method)
    (call-interactively 'cy-insert)))

(defun cyim-remember-select (&optional pos)
  (let ((rest (emms-delete-if (lambda (p) (string= (car p) "pos"))
                              (cdr cyim-current-choices))))
    (setq rest (append rest (list (cons "pos" (or pos
                                                  cyim-current-pos)))))
    (puthash cyim-current-key (cons (car cyim-current-choices)
                                   rest) (cyim-history))))

(defun cyim-number-select ()
  "如果没有可选项，插入数字，否则选择对应的词条"
  (interactive)
  (if (car cyim-current-choices)
      (let ((index (+ (cyim-page-start) (- last-command-event ?2)))
            (end (cyim-page-end)))
        (if (>= index end)
            (cyim-show)
          (cyim-remember-select (1+ index))
          (setq cyim-current-str (cyim-choice (nth index (car cyim-current-choices))))
          (cyim-terminate-translation)))
    (cyim-append-string (char-to-string last-command-event))
    (cyim-terminate-translation)))

(defun cyim-quit-clear ()
  (interactive)
  (setq cyim-current-str "")
  (cyim-terminate-translation))

(defun cyim-quit-no-clear ()
  (interactive)
  (setq cyim-current-str cyim-current-key)
  (cyim-terminate-translation))

(defun cyim-terminate-translation ()
  "Terminate the translation of the current key."
  (setq cyim-translating nil)
  (cyim-delete-region)
  (setq cyim-current-choices nil)
  (setq cyim-guidance-str "")
  (when cyim-use-tooltip
    (x-hide-tip)))

;;;_ , cyim-handle-string
(defun cyim-handle-string ()
  (if (and (functionp cyim-stop-function)
           (funcall cyim-stop-function))
      (progn
        (setq unread-command-events
              (list (aref cyim-current-key (1- (length cyim-current-key)))))
        (cyim-terminate-translation))
    (setq cyim-current-choices (cyim-get cyim-current-key)
          cyim-current-pos
          (if (cyim-get-option 'record-position)
              (cdr (assoc "pos" (cdr cyim-current-choices)))
            1))
    (cyim-format-page)))

(defun cyim-translate (char)
  (if (functionp cyim-translate-function)
      (funcall cyim-translate-function char)
    (char-to-string char)))

;;;_ , Core function of input method (stole from quail)
(defun cyim-exit-from-minibuffer ()
  (inactivate-input-method)
  (if (<= (minibuffer-depth) 1)
      (remove-hook 'minibuffer-exit-hook 'quail-exit-from-minibuffer)))

(defun cyim-setup-overlays ()
  (let ((pos (point)))
    (if (overlayp cyim-overlay)
        (move-overlay cyim-overlay pos pos)
      (setq cyim-overlay (make-overlay pos pos))
      (if input-method-highlight-flag
          (overlay-put cyim-overlay 'face 'cyim-string-face)))))

(defun cyim-delete-overlays ()
  (if (and (overlayp cyim-overlay) (overlay-start cyim-overlay))
      (delete-overlay cyim-overlay)))

(defun cyim-show ()
  (unless enable-multibyte-characters
    (setq cyim-current-key nil
          cyim-current-str nil)
    (error "Can't input characters in current unibyte buffer"))
  (cyim-delete-region)

  (setq cyim-current-length
        (length cyim-current-str))

  ;; 显示当前选择词条
  (when cyim-show-first
    (insert cyim-current-str))

  (if (eq (selected-window) (minibuffer-window))
      (insert cyim-current-str))

  (move-overlay cyim-overlay (overlay-start cyim-overlay) (point))
  ;; Then, show the guidance.
  (when (and (not input-method-use-echo-area)
             (null unread-command-events)
             (null unread-post-input-method-events))
    (if (eq (selected-window) (minibuffer-window))
        ;; Show the guidance in the next line of the currrent
        ;; minibuffer.
        (cyim-minibuffer-message
         (format "  [%s]\n%s"
                 current-input-method-title cyim-guidance-str))
      ;; Show the guidance in echo area without logging.
      (let ((message-log-max nil))
        (if cyim-use-tooltip
            (let ((pos (string-match ": " cyim-guidance-str)))
              (if pos
                  (setq cyim-guidance-str
                        (concat (substring cyim-guidance-str 0 pos)
                                "\n"
                                (make-string (/ (- (string-width cyim-guidance-str) pos) 2) (decode-char 'ucs #x2501))
                                "\n"
                                (substring cyim-guidance-str (+ pos 2)))))
              (cyim-show-tooltip cyim-guidance-str))
          (message "%s" cyim-guidance-str))))))

(defun cyim-make-guidance-frame ()
  "Make a new one-line frame for Quail guidance."
  (let* ((fparam (frame-parameters))
         (top (cdr (assq 'top fparam)))
         (border (cdr (assq 'border-width fparam)))
         (internal-border (cdr (assq 'internal-border-width fparam)))
         (newtop (- top
                    (frame-char-height) (* internal-border 2) (* border 2))))
    (if (< newtop 0)
        (setq newtop (+ top (frame-pixel-height) internal-border border)))
    (make-frame (append '((user-position . t) (height . 1)
                          (minibuffer)
                          (menu-bar-lines . 0) (tool-bar-lines . 0))
                        (cons (cons 'top newtop) fparam)))))

(defun cyim-minibuffer-message (string)
  (message nil)
  (let ((point-max (point-max))
        (inhibit-quit t))
    (save-excursion
      (goto-char point-max)
      (insert string))
    (sit-for 1000000)
    (delete-region point-max (point-max))
    (when quit-flag
      (setq quit-flag nil
            unread-command-events '(7)))))

(defun cyim-input-method (key)
  (if (or buffer-read-only
          overriding-terminal-local-map
          overriding-local-map)
      (list key)
    ;; (message "call with key: %c" key)
    (cyim-setup-overlays)
    (let ((modified-p (buffer-modified-p))
          ;; (buffer-undo-list t)      ;; cy: 注释掉了 undo all
          (inhibit-modification-hooks t))
      (unwind-protect
          (let ((input-string (cyim-start-translation key)))
            ;;   (message "input-string: %s" input-string)
            (setq cyim-guidance-str "")
            (when (and (stringp input-string)
                       (> (length input-string) 0))
              (if input-method-exit-on-first-char
                  (list (aref input-string 0))
                (cyim-input-string-to-events input-string))))
        (cyim-delete-overlays)
        (set-buffer-modified-p modified-p)
        ;; Run this hook only when the current input method doesn't
        ;; require conversion. When conversion is required, the
        ;; conversion function should run this hook at a proper
        ;; timing.
        (run-hooks 'input-method-after-insert-chunk-hook)))))

(defun cyim-start-translation (key)
  "Start translation of the typed character KEY by the current Quail package.
Return the input string."
  ;; Check the possibility of translating KEY.
  ;; If KEY is nil, we can anyway start translation.
  (if (or (integerp key) (null key))
      ;; OK, we can start translation.
      (let* ((echo-keystrokes 0)
             (help-char nil)
             (overriding-terminal-local-map (cyim-mode-map))
             (generated-events nil)
             (input-method-function nil)
             (modified-p (buffer-modified-p))
             last-command-event last-command this-command)
        (setq cyim-current-str ""
              cyim-current-key ""
              cyim-translating t)
        (if key
            (setq unread-command-events
                  (cons key unread-command-events)))
        (while cyim-translating
          (set-buffer-modified-p modified-p)
          (let* ((prompt (if input-method-use-echo-area
                             (format "%s%s %s"
                                     (or input-method-previous-message "")
                                     cyim-current-key
                                     cyim-guidance-str)))
                 (keyseq (read-key-sequence prompt nil nil t))
                 (cmd (lookup-key (cyim-mode-map) keyseq)))
            ;;             (message "key: %s, cmd:%s\nlcmd: %s, lcmdv: %s, tcmd: %s"
            ;;                      key cmd last-command last-command-event this-command)
            (if (if key
                    (commandp cmd)
                  (eq cmd 'cyim-self-insert-command))
                (progn
                  ;; (message "keyseq: %s" keyseq)
                  (setq last-command-event (aref keyseq (1- (length keyseq)))
                        last-command this-command
                        this-command cmd)
                  (setq key t)
                  (condition-case err
                      (call-interactively cmd)
                    (error (message "%s" (cdr err)) (beep))))
              ;; KEYSEQ is not defined in the translation keymap.
              ;; Let's return the event(s) to the caller.
              (setq unread-command-events
                    (string-to-list (this-single-command-raw-keys)))
              ;; (message "unread-command-events: %s" unread-command-events)
              (cyim-terminate-translation))))
        ;;    (1message "return: %s" cyim-current-str)
        cyim-current-str)
    ;; Since KEY doesn't start any translation, just return it.
    ;; But translate KEY if necessary.
    (char-to-string key)))

(defun cyim-input-string-to-events (str)
  (let ((events (mapcar
                 (lambda (c)
                   ;; This gives us the chance to unify on input
                   ;; (e.g. using ucs-tables.el).
                   (or (and translation-table-for-input
                            (aref translation-table-for-input c))
                       c))
                 str)))
    (if (or (get-text-property 0 'advice str)
            (next-single-property-change 0 'advice str))
        (setq events
              (nconc events (list (list 'cyim-advice str)))))
    events))

(defun cyim-advice (args)
  (interactive "e")
  (let* ((string (nth 1 args))
         (func (get-text-property 0 'advice string)))
    (if (functionp func)
        (funcall func string))))

(global-set-key [cyim-advice] 'cyim-advice)

;;; borrow from completion-ui
(defun cyim-frame-posn-at-point (&optional position window)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of frame containing WINDOW. Defaults
to the position of point in the selected window."
  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))
  (let ((x-y (posn-x-y (posn-at-point position window)))
        (edges (window-inside-pixel-edges window)))
    (cons (+ (car x-y) (car  edges))
          (+ (cdr x-y) (cadr edges)))))

(defface cyim-tooltip-face '((((class color)) :inherit tooltip))
  "face to display items"
  :group 'cyim)

(defun cyim-show-tooltip (text)
  "Show tooltip text near cursor."
  (let ((pos (cyim-frame-posn-at-point))
        (fg (face-attribute 'cyim-tooltip-face :foreground nil 'tooltip))
        (bg (face-attribute 'cyim-tooltip-face :background nil 'tooltip))
        (params tooltip-frame-parameters)
        ;; seem the top position should add 65 pixel to make
        ;; the text display under the baseline of cursor
        (top-adjust 65)
        (frame-height (frame-pixel-height))
        (frame-width (frame-pixel-width))
        (lines (split-string text "\n"))
        width height left top)
    (setq width (* (frame-char-width) (apply 'max (mapcar 'string-width lines)))
          height (* (frame-char-height) (length lines)))
    (setq left (frame-parameter nil 'left)
          top (frame-parameter nil 'top))
    ;; if the cursor is at near the right frame fringe or at bottom
    ;; of the bottom fringe, move the frame to
    ;; -frame-width or -frame-height from right or bottom
    (if (< (- frame-width (car pos)) width)
        (setq left (+ left (max 0 (- frame-width width))))
      (setq left (+ left (car pos))))
    (if (< (- frame-height (cdr pos)) (+ height top-adjust))
        (setq top (+ top (max 0 (- frame-height height))))
      (setq top (+ top (cdr pos))))
    (setq top (+ top top-adjust))
    (when (stringp fg)
      (setq params (append params `((foreground-color . ,fg)
                                    (border-color . ,fg)))))
    (when (stringp bg)
      (setq params (append params `((background-color . ,bg)))))
    (setq params (append params `((left . ,left) (top . ,top))))
    (x-show-tip (propertize text 'face 'cyim-tooltip-face)
                nil params cyim-tooltip-timeout)))

;;;_. utils
;;;###autoload
(defun cyim-create-word-file ()
  "创建一个能用于 cyim 的新文件，按说明填入相应的内容就能生成对应的输入法"
  (interactive)
  (let ((buffer (generate-new-buffer "cyim-word")))
    (switch-to-buffer buffer)
    (insert
     "[Comment]\n"
     "要创建一个新的 cyim 输入法文件，最简单的方法是只要在 Table 部分填入码表\n"
     "就行了。更多的设置如下：\n"
     "# 控制是否进入转换。一般设置成所有词库中的首字母\n"
     "first-char=\n"
     "# 控制是否退出转换，一般设置成所有词库中的字母\n"
     "total-char=\n"
     "# 在启动时 load 的 elisp 文件\n"
     "lib=\n"
     "# 其它词库文件，用 ; 隔开\n"
     "other-files=\n"
     "# 每页显示的词条数目\n"
     "page-length=\n\n"
     "如果需要加入标点，加入一个 Punctuation 部分。然后设置 cyim-translate-fuction。\n"
     "如果需要排序，或者合并相同编码的词条，使用 C-c C-c 或者 M-x cyim-build-table。\n"
     "如果有需要，可能还要修改 first-char 和 total-char\n\n"
     "[Parameter]\n"
     "first-char=abcdefghijklmnopqrstuvwxyz\n"
     "total-char=abcdefghijklmnopqrstuvwxyz\n\n"
     "[Description]\n"
     "\n\n"
     "[Table]\n"
     )
    (local-set-key "\C-c\C-c" 'cyim-build-table)))

;;;###autoload
(defun cyim-build-table ()
  (interactive)
  (save-restriction
    (let ((table (cyim-section-region "Table"))
          (param (cyim-section-region "Parameter"))
          (lastw "")
          first-char total-char currw)
      (narrow-to-region (car table) (cdr table))
      (perform-replace "[ \t]+$" "" nil t nil nil nil (point-min) (point-max))
      (sort-lines nil (point-min) (point-max))
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at "^[ \t]*$")     ; 如果有空行，删除
            (cyim-delete-line)
          (setq currw (cyim-code-at-point))
          (add-to-list 'first-char (aref currw 0))
          (mapc (lambda (c) (add-to-list 'total-char c)) (append currw nil))
          (if (string= currw lastw)
              (delete-region (1- (point)) (+ (point) (length currw))))
          (setq lastw currw)
          (forward-line 1)))
      (narrow-to-region (car param) (cdr param))
      (goto-char (point-min))
      (insert "first-char=" (concat first-char) "\n"
              "total-char=" (concat total-char) "\n")
      (while (not (eobp))
        (if (or (looking-at "^first-char=")
                (looking-at "^total-char="))
            (cyim-delete-line)
          (forward-line 1)))
      (if (looking-at "^$")
          (delete-backward-char 1)))))

(provide 'cyim)
;;; cyim.el ends here
