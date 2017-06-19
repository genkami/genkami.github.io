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
http://blog.fpmurphy.com/#sthash.TW0lvn5D.dpbs
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

UEFI_PXE_BASE_CODE_PROTOCOL

UEFI Spec 2.3 Calling Conventions
http://wiki.osdev.org/UEFI#UEFI_applications_in_detail

typedef
EFI_STATUS
(EFIAPI *EFI_OPEN_PROTOCOL) (
IN EFI_HANDLE Handle,
IN EFI_GUID *Protocol,
OUT VOID **Interface OPTIONAL,
IN EFI_HANDLE AgentHandle,
IN EFI_HANDLE ControllerHandle,
IN UINT32 Attributes
);

typedef
EFI_STATUS
(EFIAPI *EFI_IMAGE_LOAD) (
IN BOOLEAN BootPolicy,
IN EFI_HANDLE ParentImageHandle,
IN EFI_DEVICE_PATH_PROTOCOL *DevicePath,
IN VOID *SourceBuffer OPTIONAL,
IN UINTN SourceSize,
OUT EFI_HANDLE *ImageHandle
);

typedef
EFI_STATUS
(EFIAPI *EFI_IMAGE_START) (
IN EFI_HANDLE ImageHandle,
OUT UINTN *ExitDataSize,
OUT CHAR16 **ExitData OPTIONAL
);

https://ja.wikibooks.org/wiki/UEFI%E3%82%A2%E3%83%97%E3%83%AA%E3%82%B1%E3%83%BC%E3%82%B7%E3%83%A7%E3%83%B3%E3%81%AE%E6%9B%B8%E3%81%8D%E6%96%B9/Hello_World%E3%83%97%E3%83%AD%E3%82%B0%E3%83%A9%E3%83%A0

  Status = SystemTable->BootServices->LocateProtocol(
    &SimpleFileSystemGuid,
    NULL,
    &SimpleFileSystem
    );


 http://orumin.blogspot.jp/2014/12/4.html
 https://stackoverflow.com/questions/31188047/uefi-loadimage-from-memory
 https://stackoverflow.com/questions/31317566/uefi-loadimage-hangs

 Device Path Protocolの配列としてファイルの位置などを表す
 ASCIIZみたいな感じで、配列の終端は特殊な値を使って終端であることを明示する
   ImgPath[1].Header.Type = END_DEVICE_PATH_TYPE;
  ImgPath[1].Header.SubType = END_ENTIRE_DEVICE_PATH_SUBTYPE;
  ImgPath[1].Header.Length[0] = (UINT8)sizeof(EFI_DEVICE_PATH);
  ImgPath[1].Header.Length[1] = (UINT8)(sizeof(EFI_DEVICE_PATH) >> 8);

EFI_DEVICE_PATHは基本の方で、通常はその後ろに追加のデータがくっついた方を使う。
例えばメモリ上を表すDevice PathならMEMMAP_DEVICE_PATHとか

 Protocol\DevicePath.hに特定用途用デバイスパスの構造体と、Type, SubTypeの定数が定義されている