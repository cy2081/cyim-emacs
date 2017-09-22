- [Emacs 版本](#sec-1)
- [Linux Fcitx 版本](#sec-2)
- [Vim 版本](#sec-3)
- [Windows 版本](#sec-4)

**cyim 穿越中文输入法 Emacs 版本**

版本： 0.9

作者： cy@baow.com

穿越中文输入法是一种简单易学输入快速的汉字输入方法，目标是成为软件编程人员易于使用的中文输入法。

中文编码： <http://vim.baow.com/cyim/data/20121015181207/index.html>

# Emacs 版本<a id="sec-1"></a>

这是在Emacs中使用的中文输入法，主要程序是从 <https://github.com/zilongshanren/chinese-wbim> 移植过来的，基本设置和chinese-wbim一样，改进部分如下：

-   码表文件采用了Vim版本中的码表cy.txt，具体汉字编码方式参见： <http://vim.baow.com/cyim>
-   实现了4个字母对应一个单词时，自动上屏。
-   大写英文字母自动切换到英文模式

TODO： 加入编程词汇表。

设置方法是先把 chinese-wbim 放到 .emacs.d 目录中，然后把以下代码加入到 .emacs 文件中

```emacs-lisp

;; 添加到 load-path
(setq load-path (cons (file-truename "~/.emacs.d/chinese-wbim") load-path))

(autoload 'chinese-wbim-use-package "chinese-wubi" "Another emacs input method")

;; Tooptip is not good enough, so disable it here.
(setq chinese-wbim-use-tooltip nil)

(register-input-method
 "chinese-wubi" "euc-cn" 'chinese-wbim-use-package
 "穿越" "穿越中文输入法" "cy.txt")

(require 'chinese-wbim-extra)

;; 用 ; 暂时输入英文
(global-set-key ";" 'chinese-wbim-insert-ascii)

;; 英文大写字母自动切换到英文
(let ((i 65))
   (while (< i 91)
      (global-set-key (char-to-string i) 'chinese-cy-insert-en)
      (setq i (1+ i))))

;; 设置默认输入法
(setq default-input-method 'chinese-wubi)

;; 设置输入法开关 
(global-set-key (kbd "M-SPC") 'toggle-input-method)

;; 设置英文标点切换
(global-set-key (kbd "C-,") 'chinese-wbim-punc-translate-toggle)

```

如果使用Spacemacs，基本方式类似。

汉字的编码方案请参见Vim版中的说明：

<http://vim.baow.com/cyim/data/20121015181207/index.html>

# Linux Fcitx 版本<a id="sec-2"></a>

请把 fcitx/table 中的文件， 复制到主目录中的 .config/fcitx/table 中。可能需要重启fcitx，然后选择穿越中文输入法。

# Vim 版本<a id="sec-3"></a>

见 <http://vim.baow.com/cyim>

# Windows 版本<a id="sec-4"></a>

请从 <http://ddeb.ys168.com/> 安装兼容的奔腾输入法。
