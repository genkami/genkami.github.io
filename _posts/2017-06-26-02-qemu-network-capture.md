---
layout: post
title: QEMUのuser mode network内のパケットをキャプチャする
tags:
- Network
---

QEMUにはパケットをキャプチャしてくれる機能があるので、オプションを足すだけで簡単にできます。

```
$ qemu -net nic,model=e1000 -net dump,file=/path/to/hoge.pcap -net user
```

あとは吐かれたファイルをWiresharkなりで適当に解析すればok
