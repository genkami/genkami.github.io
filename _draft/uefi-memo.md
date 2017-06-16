---
layout: post
title: 
tags:
- 
---

\efi\boot\bootx64.efi or \efi\boot\bootia32.efi においてブート
逆にこのパスで指定される.efiファイルがないとシェルに落ちるので、そこから手動で選択するのもあり

+ map: デバイス一覧
+ FSX: FSXに移動
+ cd:
+ ls+
+ path/to/application.efi: efiを実行


http://orumin.blogspot.jp/2014/01/uefi.html
http://x86asm.net/articles/uefi-programming-first-steps/index.html
http://orumin.blogspot.jp/2013/01/grubefi.html
ここに書いてること結構役立ちそう

UEFI Protocol
プロトコルとかいってるけどちょっと書き方が特殊なクラスみたいなもの

main の引数に入っているSystemTableがだいたい何でも持ってるので、そこから必要なプロトコルを探し出して使っていく感じ
mainの型は EFI_STATUS EFIAPI UefiMain(EFI_HANDLE, EFI_SYSTEM_TABLE)

SystemTable->BootServices->LoadImage, StartImage で他のUEFI Applicationを実行できるかも

Device Path Protocol
/PciRoot(0x0)/Pci(0x1,0x1)/Ata(0x0)/HD(2,GPT,XXXX-XXXX-XXXX-XXXX,0x10080,0x32000)/MyHelloWorld.efi
ConvertTextToDevicePath で DevicePathProtocolに変換できる
それをLoadImage()して、StartImage()すれば引っ張ってきたUEFIイメージに制御を移せる
