(setq cyim-emacs-packages
      '(
        (cyim :location local :toggle (eq chinese-default-input-method 'cyim))))

(defun cyim-emacs/init-cyim ()
  "Initialize chinese-cyim"
  (use-package cyim
    :if (eq 'cyim chinese-default-input-method)
    :init
    (progn

      (autoload 'cyim-use-package "cyim" "CY input method")
      (register-input-method "cyim" "euc-cn" 'cyim-use-package
                             "穿越" "穿越中文输入法" "cy-table.txt")

      (require 'cyim-extra)

      ;; 设置 return 选择第一项
      ;; (add-hook 'cyim-cy-load-hook
      ;;           (lambda ()
      ;;             (let ((map (cyim-mode-map)))
      ;;               (define-key map [return] 'cyim-select-current))))


      ;; 设置光标跟随移动提示， t 或 nil
      (setq cyim-use-tooltip nil)

      ;; 打开输入空格时自动切换到英文状态
      (setq cyim-quick-en t)

      ;; 设置当前显示第一项
      (setq cyim-show-first nil)

      ;; 设置英文切换快捷键
      (global-set-key (kbd "C-;") 'cyim-insert-ascii)

      ;; 设置中英文切换快捷键， linux 中就是 Alt + Space
      (global-set-key (kbd "M-SPC") 'cyim-toggle)

      ;; 设置中英文标点切换快捷键
      (global-set-key (kbd "C-,") 'cyim-punc-translate-toggle)

      ;; 删除已经输入的单词
      (global-set-key (kbd "M-u") 'cyim-delete-last-word)

      ;; 设置为默认输入法
      (setq default-input-method 'cyim)

      ;; hybrid 模式时，遇到括号自动切换英文
      (add-hook 'evil-hybrid-state-entry-hook 'cyim-evil-insert-toggle)

      ;; minibuffer 中输入时关闭中文输入法
      (add-hook 'minibuffer-setup-hook 'deactivate-input-method)

      ;; emacs-lisp 配合 lispyville 时，自动切换英文
      (add-hook 'emacs-lisp-mode-hook
                (lambda ()
                  (when (and (boundp lispyville-mode) lispyville-mode)
                    (add-hook 'activate-mark-hook #'cyim-evil-normal-toggle :local t))))

      ;; scheme 配合 lispyville 时，自动切换英文
      (add-hook 'scheme-mode-hook
                (lambda ()
                  (when (and (boundp lispyville-mode) lispyville-mode)
                    (add-hook 'activate-mark-hook #'cyim-evil-normal-toggle :local t))
                  )))))
