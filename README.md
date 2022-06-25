注意：以下为旧文件，已简化升级为[呼吸输入法](https://github.com/huxi-fun/huxi)，详见

https://github.com/huxi-fun/huxi

--------
# 穿越中文输入法 Emacs 版 (Cyim-emacs)

版本： 1.2

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
 - [2. 安装设置](#sec-1)
   - [2.1 use-package](#sec-21)
   - [2.2 Spacemacs](#sec-22)
 - [3. 快捷键](#sec-3)
 - [4. 词库](#sec-4)
 - [5. 其它版本](#sec-5)
     - [5.1 Android 手机版](#sec-51)
     - [5.2 Vim 版](#sec-52)

## 1. 中文编码<a id="sec-0"></a>

[小小音形输入法](http://xxyx.ys168.com/)

## 2. 安装<a id="sec-1"></a>


```bash
git clone https://github.com/cy2081/cyim-emacs.git

```

先把 `local/cyim` 目录放到 `.emacs.d` 中，然后按照下边进行设置。

## 2.1 use-package <a id="sec-21"></a>

见 `use.el` 中的代码。

## 2.2 Spacemacs 设置<a id="sec-22"></a>

如果使用 Spacemacs，可以直接把 `cyim-emacs` 当作一个新 layer 添加到 `~/.emacs.d/private` 中，配置文件 `config.el` 和 `packages.el` 已经建好。

然后在 `.spacemacs` 的 `dotspacemacs-configuration-layer` 中加入：

```emacs-lisp
(cyim-emacs :variables chinese-default-input-method 'cyim)` 
```
具体配置参见 `packages.el` 。

## 3. 快捷键<a id="sec-3"></a>

`M-SPC` 切换输入法

`C-,` 切换中英文标点

`C-e` 输入英文

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

## 4. 词库<a id="sec-4"></a>
### 4.1 默认词库

直接编辑 `cy-table.txt` 即可。

### 4.2 自定义词库

把 `mycy.txt` 文件复制到 `.emacs.d` 目录下，直接编辑即可。

## 5. 其它版本<a id="sec-5"></a>
### 5.1 Android 手机版<a id="sec-51"></a>

先安装 `Termux` 和黑客键盘（Hacker’s Keyboard），再安装 Emacs，再按照上边的方法配置。

### 5.2 Vim 版<a id="sec-52"></a>

详见 <https://github.com/cy2081/vim-cyim>
