- [Emacs 版本](#sec-1)
- [Android 手机](#sec-2)
- [Linux Fcitx 版本](#sec-3)
- [Vim 版本](#sec-4)
- [感谢](#sec-5)

**穿越中文输入法 Emacs 版本**

版本： 1.0

作者： cy@baow.com

穿越中文输入法是一种简单易学输入快速的汉字输入方法，目标是成为软件编程人员易于使用的中文输入法。

中文编码方法见 docs 目录中的 [chinese-cy.html](docs/chinese-cy.html)

# Emacs 版本<a id="sec-1"></a>

这是在Emacs中使用的中文输入法，输入中文比较快捷方便。特点有：

 - 遇到空格和大写字母自动切换到英文输入
 - 实现了4个字母对应一个单词时，自动上屏。

安装方法是先把 cyim 放到 .emacs.d 目录中，然后把以下代码加入到 .emacs 文件中：

```emacs-lisp
;; 添加到 load-path
(setq load-path (cons (file-truename "~/.emacs.d/cyim") load-path))

(autoload 'cyim-use-package "cyim" "CY input method")
(register-input-method "cyim" "euc-cn" 'cyim-use-package
                       "穿越" "穿越中文输入法" "cy-table.txt")

(add-hook 'cyim-wb-load-hook
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

;; 设置为默认输入法
(setq default-input-method 'cyim)

```

如果使用Spacemacs，基本方式类似。

## 快捷键

M-SPC  切换输入法

C-,  切换中英文标点

C-;  输入英文

C-n 选项下一页

C-p 选项上一页

C-m 输入字母

C-c 或 C-g 取消输入

SPC  选择第一项

[  选择第二项

‘  选择第三项

## 自定义词库

把 mycy.txt 文件复制到 .emacs.d 目录下，直接编辑即可。

## 词库管理

直接编辑 cy-table.txt 即可。


# Android 手机<a id="sec-2"></a>

先安装Termux和黑客键盘（Hacker’s Keyboard），再安装Emacs，再按照上边的方法配置。

# Linux Fcitx 版本<a id="sec-3"></a>

请把 fcitx/table 中的文件， 复制到主目录中的 .config/fcitx/table 中。可能需要重启fcitx，然后选择穿越中文输入法。

# Vim 版本<a id="sec-4"></a>

见 <http://vim.baow.com/cyim>

# 感谢<a id="sec-5"></a>

感谢众多开演软件作者，谢谢大家的支持和努力。
