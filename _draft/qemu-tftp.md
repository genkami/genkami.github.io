QEMU付属のTFTPサーバーを使う
ユーザーモードネットワーク
でふぉるとではDHCPサーバーが動いている
-net user,tftp=dir
dirをtftpのrootにできる
-net nic -netdev user,id=tftptest,tftp=C:\TFTPROOT
これだと動かなかった。wikiの記述によるとなにかたりない？後で調べる
