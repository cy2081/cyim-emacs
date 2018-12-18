;; 添加到 load-path
(setq load-path (cons (file-truename "~/.emacs.d/cyim") load-path))

(autoload 'cyim-use-package "cyim" "CY input method")
(register-input-method "cyim" "euc-cn" 'cyim-use-package
                       "穿越" "穿越中文输入法" "cy-table.txt")

(add-hook 'cyim-cy-load-hook
          (lambda ()
            (let ((map (cyim-mode-map)))
              (define-key map [return] 'cyim-select-current))))

(require 'cyim-extra)

;; 打开光标跟随移动提示
(setq cyim-use-tooltip t)

;; 打开输入空格时自动切换到英文状态
(setq cyim-quick-en t)

;; 设置英文切换快捷键
(global-set-key (kbd "C-;") 'cyim-insert-ascii)

;; 设置中英文切换快捷键
(global-set-key (kbd "M-SPC") 'toggle-input-method)

;; 设置中英文标点切换快捷键
(global-set-key (kbd "C-,") 'cyim-punc-translate-toggle)

;; 取消当前输入，并切换到英文
(global-set-key (kbd "C-g") 'cyim-clear-toggle)

;; 设置为默认输入法
(setq default-input-method 'cyim)
