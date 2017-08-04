---
layout: post
title: 
tags:
- 
---

まずはubuntuを入れる

以下ubuntuでの操作

https://www.bitvisor.org/

ダウンロード
$ hg clone https://bitbucket.org/bitvisor/bitvisor
$ cd bitvisor
$ make
$ make -C boot/uefi-loader

できたファイルをEFIパーティションにコピー

# mkdir /boot/efi/EFI/bitvisor
# cp bitvisor.elf /boot/efi/EFI/bitvisor
# cp boot/uefi-loader/loadvmm.efi /boot/efi/EFI/bitvisor

rEFIndのメニューから起動できるようにする

/boot/efi/EFI/refind/refind.conf

menuentry "BitVisor" {
    loader \EFI\bitvisor\loadvmm.efi
}


reboot

rEFInd
Load BitVisor from EFI

一瞬でrEFIndに戻ってきたらたぶん成功

Ubuntu上から
cd bitvisor/tools/dbgsh
make
./dbgsh
> log

でログが見れる

