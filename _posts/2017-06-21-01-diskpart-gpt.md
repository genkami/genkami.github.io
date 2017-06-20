---
layout: post
title: UEFIアプリケーションが起動できる仮想ディスクを作る
tags:
- UEFI
---
今までBIOSしか使ってなかったんでMBRとかそういう系の話しか知りませんでしたが、UEFIでのブートプロセスはBIOSのものとはかなり違うようです。

UEFIでは旧来のMBRに代わり、GPTを使ってパーティションを切られたディスクが必要になります。

GPTを使った仮想ディスクの作成は、基本的に以下のサイトに従えばなんとかなります。

[サンプル: Windows PE と DiskPart を使って UEFI/GPT ベースのハード ドライブ パーティションを構成する](https://msdn.microsoft.com/ja-jp/library/hh825686.aspx)

```
DISKPART> create vdisk file="c:\hello.vhd" maximum=200 type=expandable
DISKPART> list disk

  ディスク      状態           サイズ   空き   ダイナ GPT
  ###                                          ミック
  ------------  -------------  -------  -------  ---  ---
  ディスク 0    オンライン            70 GB  1024 KB

DISKPART> select vdisk file="C:\hello.vhd"
DISKPART> attach vdisk
DISKPART> list disk

  ディスク      状態           サイズ   空き   ダイナ GPT
  ###                                          ミック
  ------------  -------------  -------  -------  ---  ---
  ディスク 0    オンライン            70 GB  1024 KB
* ディスク 1    オンライン           200 MB   200 MB
DISKPART> convert gpt
DISKPART> create partition efi size=100
DISKPART> list partition

  Partition ###  Type                Size     Offset
  -------------  ------------------  -------  -------
  Partition 1    予約                  32 MB    17 KB
* Partition 2    システム               100 MB    32 MB

DISKPART> format quick fs=fat32 label="System"
```

とりあえずUEFIアプリケーションを適当に作って実行したいだけなので、起動用のシステムパーティションを1つだけ切りました。

次にこれをマウントします。

```
DISKPART> assign letter="S"
```

Sドライブに先ほどの仮想ディスクをマウントしました。

UEFIは標準でシステムパーティションの`/efi/boot/bootx64.efi`を読み込んでくれるので、その位置に適当に作った.efiファイルを置いておきます。

```
DISKPART> remove letter="S"
DISKPART> detach vdisk
```

仮想ディスクをアンマウントして、あとはVirtualBoxあたりを適当に起動すれば自分の作ったUEFIアプリケーションが動作しているのがわかるはずです。