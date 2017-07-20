---
layout: post
title: UEFIで開発を始めるための資料など
tags:
- UEFI
---

UEFI関連は日本語の資料が少ないので苦労します。

[Specifications \| Unified Extensible Firmware Interface Forum](http://www.uefi.org/specifications)

UEFIやPIの仕様書など。とりあえず読むべき。とはいっても何千ページもあるので必要な所から少しずつつまんでいくくらいが良さそうです。

[EDK2](https://github.com/tianocore/edk2)

UEFIの開発キット。独自の作法がいろいろとあって最初は面倒ですが、後述するgnu-efiよりもUEFIの仕様に準拠しているのでこちらのほうがおすすめです。ライブラリが充実していますが、まともなドキュメントは存在しないようです。

[gnu-efi](https://sourceforge.net/projects/gnu-efi/)

こちらもUEFIの開発キット。EDK2よりUNIXの世界と親和性が高くとっつきやすそうですが、関数をラッパ経由で呼び出さなければいけないなど、独自仕様があるところが△。こちらはほとんど使っていないので細かい所はわかりません。

[UEFI · tianocore/tianocore.github.io Wiki](https://github.com/tianocore/tianocore.github.io/wiki/UEFI)

tianocoreのwiki。EDK2を使っているなら必読。とりあえず最初は以下の記事を呼んでおくといいでしょう。
+ [Getting Started with EDK II · tianocore/tianocore.github.io Wiki](https://github.com/tianocore/tianocore.github.io/wiki/Getting-Started-with-EDK-II): とりあえずEDK2突っ込んで始めるときのガイド
+ [EDK II Specifications · tianocore/tianocore.github.io Wiki](https://github.com/tianocore/tianocore.github.io/wiki/EDK-II-Specifications): EDK2のお作法はこちら

[UEFI - PhoenixWiki](http://wiki.phoenix.com/wiki/index.php/UEFI)

Phoenixのwiki。ほげほげプロトコルの型ってどんなんだったっけ？みたいなのをさっと調べるのに便利です。

[Expanded Main Page - OSDev Wiki](http://wiki.osdev.org/Main_Page)

UEFIというかOS開発用のwikiですが、UEFIに触る人は多かれ少なかれOSに足を突っ込むことになるので必要になるはず。UEFI自体についての記述も少しあります。

[EDK II User Documentation · tianocore/tianocore.github.io Wiki](https://github.com/tianocore/tianocore.github.io/wiki/EDK-II-User-Documentation)

EDK2でモジュールやドライバを開発していくときに参考になる資料のまとめ。

まだ全然目を通せてないですが、少しずつ読んでいきながらUEFIで遊んでいきたいです。
