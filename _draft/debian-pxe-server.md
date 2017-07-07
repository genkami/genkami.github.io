debian-pxe-server

qemuの-netで作ったネットワーク内に、debianのdhcp+tftpサーバーとuefiのpxeクライアントを置く

https://www.debian.org/releases/stable/i386/ch04s05.html.ja
http://frsyuki.hatenablog.com/entry/20080720/p2
このへんを参考に

```
$ svn co http://svn.coderepos.org/share/lang/c/pxe-pdhcp
$ cd pxe-pdhcp
$ make
```


```
# apt install tftpd-hpa
```
