(use-package cyim
  :load-path "~/.emacs.d/local/cyim"
  :config
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

  ;; 设置中英文切换快捷键， linux 中就是 Alt + Space
  (global-set-key (kbd "M-SPC") 'cyim-toggle)
  (global-set-key (kbd "C-0") 'cyim-toggle)
  (global-set-key (kbd "M-3") 'cyim-toggle)

  ;; 设置临时输入英文快捷键
  (global-set-key (kbd "C-e") 'cyim-insert-ascii)

  ;; 设置中英文标点切换快捷键
  (global-set-key (kbd "C-,") 'cyim-punc-translate-toggle)

  ;; 删除已经输入的单词
  (global-set-key (kbd "M-u") 'cyim-delete-last-word)

  ;; 设置为默认输入法
  (setq default-input-method 'cyim)

  (add-hook 'input-method-activate-hook
            (lambda ()
              (set-cursor-color "DeepSkyBlue")
              (setq-local evil-normal-state-cursor '("DeepSkyBlue" box))))

  (add-hook 'input-method-inactivate-hook
            (lambda ()
              (set-cursor-color "orange")
              (setq-local evil-normal-state-cursor '("orange" box))))

  ;; insert 模式时，遇到括号自动切换英文
  (add-hook 'evil-insert-state-entry-hook 'cyim-evil-insert-entry-toggle)
  
  ;; minibuffer 中输入时关闭中文输入法
  (add-hook 'minibuffer-setup-hook 'deactivate-input-method)

  (setq default-input-method 'cyim))
