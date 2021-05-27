# 穿越中文输入法 Emacs 版 (Cyim-emacs)

版本： 1.1

作者： huxifun@sina.com

Cyim-emacs 是在 Emacs 中使用的中文输入法，输入中文比较快捷方便。除了具有基本的功能外，主要特点有：

 - 软件编程人员易于使用的中文输入法
 - 完善的中文编码，重码率低。
 - 中英文切换方便，输入更为流畅。
 - 遇到空格和大写字母自动切换到英文输入。
 - 支持遇到括号时自动切换到英文输入。
 - 实现了4个字母对应一个单词时，自动上屏。
 - 方便进行自定义，有历史记录。
 
**目录：** 

 - [1. 中文编码](#sec-0)
 - [2. 安装](#sec-1)
 - [3. 设置](#sec-2)
     - [3.1 普通设置](#sec-21)
     - [3.2 Spacemacs 设置](#sec-22)
 - [4. 快捷键](#sec-3)
 - [5. 词库](#sec-4)
 - [6. 其它版本](#sec-5)
     - [6.1 Android 手机版](#sec-51)
     - [6.2 Vim 版](#sec-53)
 - [7. 感谢](#sec-6)

## 1. 中文编码<a id="sec-0"></a>

中文编码方法见 `docs` 文档目录， 请参考 [小小音形输入法](http://xxyx.ys168.com/)

## 2. 安装<a id="sec-1"></a>

默认使用 Linux 系统。

```bash
git clone https://github.com/cy2081/cyim-emacs.git

```

## 3. 设置<a id="sec-2"></a>
## 3.1 普通设置<a id="sec-21"></a>

先把 `local/cyim` 目录放到 `.emacs.d` 中，然后按照下边进行设置。

复制 `emacs.el` 中的代码，加入到 Emacs 的启动文件 `.emacs` 中，或直接添加下面的代码：

```emacs-lisp
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
(global-set-key (kbd "C-M-;") 'cyim-insert-ascii)

;; 设置中英文标点切换快捷键
(global-set-key (kbd "C-,") 'cyim-punc-translate-toggle)

;; 删除已经输入的单词
(global-set-key (kbd "M-u") 'cyim-delete-last-word)

;; 设置为默认输入法
(setq default-input-method 'cyim)

;; 使用 Evil 的 hybrid 模式时，遇到括号自动切换英文
;; (add-hook 'evil-hybrid-state-entry-hook 'cyim-evil-insert-toggle)
```

## 3.2 Spacemacs 设置<a id="sec-22"></a>

如果使用 Spacemacs，可以直接把 `cyim-emacs` 当作一个新 layer 添加到 `~/.emacs.d/private` 中，配置文件 `config.el` 和 `packages.el` 已经建好。

然后在 `.spacemacs` 的 `dotspacemacs-configuration-layer` 中加入：

```emacs-lisp
(cyim-emacs :variables chinese-default-input-method 'cyim)` 
```
具体配置参见 `packages.el` 。

## 4. 快捷键<a id="sec-3"></a>

`M-SPC` 切换输入法

`C-,` 切换中英文标点

`C-M-;` 输入英文

`C-n` 选项下一页

`C-p` 选项上一页

`C-m` 输入字母

`C-c` 取消输入

`C-g` 取消当前输入，并切换到英文

`M-u` 删除已经输入的单词

`C-z` 删除选项中前一个字母

`SPC` 选择第一项

`[`  选择第二项

`‘`  选择第三项

## 5. 词库<a id="sec-4"></a>
### 5.1 默认词库

直接编辑 `cy-table.txt` 即可。

### 5.2 自定义词库

把 `mycy.txt` 文件复制到 `.emacs.d` 目录下，直接编辑即可。

## 6. 其它版本<a id="sec-5"></a>
### 6.1 Android 手机版<a id="sec-51"></a>

先安装 `Termux` 和黑客键盘（Hacker’s Keyboard），再安装 Emacs，再按照上边的方法配置。

### 6.2 Vim 版<a id="sec-53"></a>

详见 <https://github.com/cy2081/vim-cyim>

## 7. 感谢<a id="sec-6"></a>

感谢众多开演软件作者，谢谢大家的支持和努力。
