---
layout: post
title: 
tags:
- 
---

とりあえずiPXEが動くようになることを目標にする

http://forum.ipxe.org/showthread.php?tid=7323

https://github.com/Piker-Alpha/macosxbootloader/tree/master

https://github.com/ipxe/ipxe/pull/54

Apple Netboot Protocol は何をするもの？
MacOSを起動させるためのものだったら意味ない
UEFI上でいい感じにできる何かだったらうれしい


https://www.fink.org/netboot/netbooting.html
macからnetbootでlinuxを起動しているらしい
netboot server が ipxe を配信するようにして、 ipxeからlinuxを起動している
けど、netboot serverからipxeを起動することによってネットワークの問題が解決するのかはよくわからない

https://www.insyde.com/press_news/blog/uefi-24-review-part-4-require-network-drivers-return-efinomedia
やっぱりSNPはEFI_NO_MEDIAを返さないと仕様では決まっている


http://forum.ipxe.org/showthread.php?tid=8296&highlight=snponly
まさにこの問題。


iMac 27'' Retina 5K Late 2015, bought in May 2016 (iMac17,1):
Boot ROM: IM171.0105.B05
SMC Version: 2.33f10

Ethernet:
Broadcom 57766-A1:
Manufacturer ID: 0x14e4
Device ID: 0x1686
Versions-ID: 0x0001
Kext-Name: AppleBCM5701Ethernet.kext
Firmware-Version: 57766a-v1.15, 0xad0d59c9
Version: 10.1.12

OS: 10.11.3


iMac 27'' Retina 5K Late 2015, bought in March 2017 (iMac17,1):
Boot ROM: IM171.0105.B15
SMC Version: 2.34f2

Ethernet:
Broadcom 57766-A1:
Manufacturer ID: 0x14e4
Device ID: 0x1686
Versions-ID: 0x0001
Kext-Name: AppleBCM5701Ethernet.kext
Firmware-Version: 57766a-v1.15, 0xad0d59c9
Version: 10.2.7

OS: 10.12.3

上のバージョンだと動くらしい

https://forums.fogproject.org/topic/9269/imac-27-ipxe-boot

ここでも議論されているが、解決策は見えず

https://forums.fogproject.org/topic/10261/macos-update-broke-ipxe
https://forums.fogproject.org/topic/9278/fog-1-3-0-pb-with-mac-netboot/19
