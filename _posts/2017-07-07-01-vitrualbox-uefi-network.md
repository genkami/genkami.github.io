---
layout: post
title: VirtualBox上でUEFIからインターネット接続
tags:
- UEFI
---

EDK2付属のOVMFの環境では何もしなくてもネットに接続することができますが、VirtualBoxのUEFIはネットワークカードのドライバを標準で提供してくれていないようなので、自前で用意しなければなりません。

[https://downloadcenter.intel.com/ja/download/19186/Intel-Ethernet-Connections-Boot-Utility-Preboot-Images-and-EFI-Drivers](https://downloadcenter.intel.com/ja/download/19186/Intel-Ethernet-Connections-Boot-Utility-Preboot-Images-and-EFI-Drivers)

ここから`PREBOOT.EXE`を落として展開すると、`/APPS/EFI/EFIx64/E3522X2.EFI`というファイルがあると思います。これが必要なドライバになるので、UEFIから読み込める所においておいて、シェル上で`load`するなりアプリケーション内で`LoadImage()`するなりすればネットワークに接続できるようになります。
