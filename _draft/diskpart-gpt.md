---
layout: post
title: 
tags:
- UEFI
---
https://msdn.microsoft.com/ja-jp/library/hh825686.aspx

```
DISKPART> create vdisk file="c:\edk2\hello.vhd" maximum=200 type=expandable
DISKPART> list disk

  ディスク      状態           サイズ   空き   ダイナ GPT
  ###                                          ミック
  ------------  -------------  -------  -------  ---  ---
  ディスク 0    オンライン            70 GB  1024 KB

DISKPART> select vdisk file="C:\edk2\hello.vhd"
DISKPART> attach vdisk
DISKPART> list disk

  ディスク      状態           サイズ   空き   ダイナ GPT
  ###                                          ミック
  ------------  -------------  -------  -------  ---  ---
  ディスク 0    オンライン            70 GB  1024 KB
* ディスク 1    オンライン           200 MB   200 MB
DISKPART> convert gpt

DiskPart は選択されたディスクを GPT フォーマットに正常に変換しました。

DISKPART> create partition efi size=100
DISKPART> list partition

  Partition ###  Type                Size     Offset
  -------------  ------------------  -------  -------
  Partition 1    予約                  32 MB    17 KB
* Partition 2    システム               100 MB    32 MB

DISKPART> format quick fs=fat32 label="System"
DISKPART> assign letter="S"
DISKPART> remove letter="S"
DISKPART> detach vdisk
DISKPART> select vdisk file="C:\edk2\hello.vhd"
DISKPART> attach vdisk
```