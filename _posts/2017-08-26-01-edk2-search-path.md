---
layout: post
title: EDK2で任意のディレクトリにライブラリを置く
tags:
- UEFI
---

環境変数`PACKAGE_PATH`を指定すると、edk2のルートディレクトリ以外にもパッケージを置けるようになるようです。

```
PACKAGE_PATH=/path/to/hoge:/path/to/fuga
PACKAGE_PATH=C:\hoge;C:\fuga # Windows の場合
```

これを指定した状態で、`/path/to/hoge/HogeLib/HogeLib.inf`を作成し、`.dsc`ファイルに

```
[LibraryClasses]
  HogeLib|HogeLib/HogeLib.inf
```

と指定すると、実際に`/path/to/hoge/HogeLib/`に置かれたライブラリを読み込んでくれるようになります。

