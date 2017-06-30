QEMU付属のTFTPサーバーを使う
ユーザーモードネットワーク
でふぉるとではDHCPサーバーが動いている
-net user,tftp=dir
dirをtftpのrootにできる
-net nic -netdev user,id=tftptest,tftp=C:\TFTPROOT
これだと動かなかった。wikiの記述によるとなにかたりない？後で調べる

qemu-system-i386 -L . -hda C:\edk2\hello.vhd -bios C:\projs\edk2\Build\OvmfIa32\DEBUG_VS2015x86\FV\OVMF.fd -net dump,file=qemu.pcap -net nic,model=e1000 -netdev user,id=pxeboot,tftp=C:\TFTPROOT,net=192.168.5.0/24


memo:
10:50 <marushin> Option66&67&11&17の設定ですね