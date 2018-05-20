---
layout: post
title: gitignore.ioを使って.gitignoreを自動生成する
tags:
- Git
---

.gitignoreを手動で用意したり、[.gitignoreのテンプレート集](https://github.com/github/gitignore)をコピペする時代は終わりました。

[gitignore.io](https://www.gitignore.io/)

![gitignore.io](/img/post/2018-05-20-gitignore.png)

単純な使い方としては、上のサイトの検索窓に適当なワード(例: Emacsを使ってPythonのコードを書く場合、 「Emacs」「Python」等)を入力し、「Create」ボタンを押すだけで対応する.gitignoreを生成してくれます。

さらに、gitignore.ioは簡単なAPIを提供しており、以下のような関数を `.bashrc` に追加するだけで簡単に.gitignoreを生成することができます。

``` sh
function gi() {
    curl -L -s https://www.gitignore.io/api/$@
}
```

このコマンドを使えば、以下のように一行で.gitignoreを生成することができます。


``` sh
$ gi emacs,python > .gitignore
$ cat .gitignore

# Created by https://www.gitignore.io/api/emacs,python

### Emacs ###
# -*- mode: gitignore; -*-
*~
\#*\#
/.emacs.desktop
/.emacs.desktop.lock
*.elc

...

### Python ###
# Byte-compiled / optimized / DLL files
__pycache__/
*.py[cod]
*$py.class

# C extensions
*.so

...
```

