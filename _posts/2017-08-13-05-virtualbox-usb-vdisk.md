---
layout: post
title: VirtualBoxでHDDやUSBメモリを仮想ディスクとしてマウント
tags:
- Others
---

`VBoxManage internalcommands createrawvmdk`コマンドで、ホストの物理的なディスクを直接読み書きする仮想ディスクを作ることができます。

```
$ sudo VBoxManage internalcommands createrawvmdk -filename hoge.vmdk -rawdisk /dev/hoge
```

後は作られた`hoge.vmdk`を通常の仮想ディスクと同様に扱えばokです。
