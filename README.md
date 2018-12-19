
# 穿越中文输入法 Emacs 版

 - [中文编码](#sec-0)
 - [安装](#sec-1)
 - [设置](#sec-2)
 - [快捷键](#sec-3)
 - [词库](#sec-4)
 - [其它版本](#sec-5)
     - [Android 手机版](#sec-51)
     - [Linux Fcitx 版](#sec-52)
     - [Vim 版](#sec-53)
 - [感谢](#sec-6)

版本： 1.0

作者： cy@baow.com

穿越中文输入法是一种简单易学输入快速的汉字输入方法，目标是成为软件编程人员易于使用的中文输入法。

这是在 Emacs 中使用的中文输入法，输入中文比较快捷方便。除了具有基本的功能外，主要特点有：

 - 完善中英文切换方式，输入更为流畅。
 - 遇到空格和大写字母自动切换到英文输入。
 - 实现了4个字母对应一个单词时，自动上屏。

## 中文编码<a id="sec-0"></a>

中文编码方法见 docs 文档目录中的 [chinese-cy.html](docs/chinese-cy.html)

## 安装<a id="sec-1"></a>

默认使用 Linux 系统，安装方法是先把 cyim 目录放到 .emacs.d 中，然后把以下边的设置代码。

## 设置<a id="sec-2"></a>

复制 `emacs.el` 中的代码加入到 Emacs 的启动文件 `.emacs` 中，或直接添加下面的代码：

```emacs-lisp
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

```

如果使用 Spacemacs，基本方式类似。

## 快捷键<a id="sec-3"></a>

`M-SPC` 切换输入法

`C-,` 切换中英文标点

`C-;` 输入英文

`C-n` 选项下一页

`C-p` 选项上一页

`C-m` 输入字母

`C-c` 取消输入

`C-g` 取消当前输入，并切换到英文

`SPC` 选择第一项

`[`  选择第二项

`‘`  选择第三项

## 词库<a id="sec-4"></a>
### 默认词库

直接编辑 cy-table.txt 即可。

### 自定义词库

把 mycy.txt 文件复制到 .emacs.d 目录下，直接编辑即可。

## 其它版本<a id="sec-5"></a>
### Android 手机版<a id="sec-51"></a>

先安装 Termux 和黑客键盘（Hacker’s Keyboard），再安装 Emacs，再按照上边的方法配置。

### Linux Fcitx 版<a id="sec-52"></a>

请把 `fcitx/table 中的文件`， 复制到主目录中的 `.config/fcitx/table` 中。可能需要重启fcitx，然后选择穿越中文输入法。

### Vim 版<a id="sec-53"></a>

详见 <http://vim.baow.com/cyim>

## 感谢<a id="sec-6"></a>

感谢众多开演软件作者，谢谢大家的支持和努力。
