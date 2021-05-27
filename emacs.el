;; 添加到 load-path
(push "~/.emacs.d/local/cyim" load-path)

(autoload 'cyim-use-package "cyim" "CY input method")
(register-input-method "cyim" "euc-cn" 'cyim-use-package
                       "穿越" "穿越中文输入法" "cy-table.txt")

;; 设置 return 选择第一项
;; (add-hook 'cyim-cy-load-hook
;;           (lambda ()
;;             (let ((map (cyim-mode-map)))
;;               (define-key map [return] 'cyim-select-current))))

(require 'cyim-extra)

;; 设置光标跟随移动提示， t 或 nil
(setq cyim-use-tooltip nil)

;; 打开输入空格时自动切换到英文状态
(setq cyim-quick-en t)

;; 设置当前显示第一项
(setq cyim-show-first nil)

;; 设置中英文切换快捷键， linux 中就是 Alt + Space
(global-set-key (kbd "M-SPC") 'toggle-input-method)

;; 设置临时输入英文快捷键
(global-set-key (kbd "C-e") 'cyim-insert-ascii)

;; 设置中英文标点切换快捷键
(global-set-key (kbd "C-,") 'cyim-punc-translate-toggle)

;; 删除已经输入的单词
(global-set-key (kbd "M-u") 'cyim-delete-last-word)

;; 设置为默认输入法
(setq default-input-method 'cyim)

;; 使用 Evil 的 hybrid 模式时，遇到括号自动切换英文
;; (add-hook 'evil-hybrid-state-entry-hook 'cyim-evil-insert-toggle)
