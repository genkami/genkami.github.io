---
layout: post
title: MacでEFIパーティションをマウントする
tags:
- UEFI
- Mac
---

EFIパーティションはFinder上からは通常は見ることはできませんが、コマンドライン上では普通にマウントできるようです。

Macでは`diskutil`コマンドでハードディスクやパーティションの情報を見ることができます。

```
$ diskutil list
/dev/disk0 (internal, physical):
   #:                       TYPE NAME                    SIZE       IDENTIFIER
   0:      GUID_partition_scheme                        *251.0 GB   disk0
   1:                        EFI EFI                     209.7 MB   disk0s1
   2:                  Apple_HFS Macintosh HD            159.2 GB   disk0s2
   3:                 Apple_Boot Recovery HD             650.0 MB   disk0s3
   4:       Microsoft Basic Data BOOTCAMP                90.8 GB    disk0s4

```

これでEFIパーティションを特定すれば、あとはマウントするだけです。

```
$ sudo mkdir /Volumes/efi
sudo mount -t msdos /dev/disk0s1 /Volumes/efi
```

