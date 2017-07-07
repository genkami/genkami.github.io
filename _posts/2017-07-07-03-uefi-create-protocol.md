---
layout: post
title: UEFIで自作プロトコルをインストールする
tags:
- UEFI
- C
---

今までは用意された`EFI_HOGE_PROTOCOL`みたいなのを使うだけでしたが、自分で新しいプロトコルを作ることもできます。

プロトコルを作るのには以下の2つが必要になります。

1. プロトコルのGUID
2. プロトコルを表す構造体

GUIDについては、[guidgen.com](http://guidgen.com/)などを使って生成したものを使うといいでしょう。

今回は、Hello Worldを表示するだけのプロトコルである`HELLO_WORLD_PROTOCOL`を作ってみました。

``` c
#define HELLO_WORLD_PROTOCOL_GUID \
  { 0x1020e163, 0x691d, 0x4202, { 0xa7, 0x76, 0x70, 0x73, 0x64, 0x36, 0x23, 0xbd } }

EFI_GUID gHelloWorldProtocolGuid = HELLO_WORLD_PROTOCOL_GUID;

typedef EFI_STATUS (EFIAPI *HELLO_WORLD)();

typedef struct HELLO_WORLD_PROTOCOL {
  HELLO_WORLD HelloWorld;
} HELLO_WORLD_PROTOCOL;
```

作ったプロトコルは、`EFI_BOOT_SERVICES.InstallMultipleProtocolInterfaces()`を使ってインストールします。

``` c
InstallMultipleProtocolInterfaces(&Handle, &guid1, &protocol1, &guid2, &protocol2, ...);
```

この関数を使うと、`Handle`に複数のプロトコルをインストールすることができます。

ちなみに、`Handle`が`NULL`出会った場合、`Handle`は新しく確保されます。


``` c
#include <Uefi.h>
#include <Library/UefiApplicationEntryPoint.h>
#include <Library/UefiLib.h>

EFI_HANDLE gIH;
EFI_SYSTEM_TABLE *gST;
EFI_BOOT_SERVICES *gBS;

EFI_STATUS
EFIAPI
MyHelloWorld (
  IN HELLO_WORLD_PROTOCOL *This
  )
{
  return gST->ConOut->OutputString(gST->ConOut, L"Hello, world!\r\n");
}

EFI_STATUS
EFIAPI
UefiMain (
  IN EFI_HANDLE        ImageHandle,
  IN EFI_SYSTEM_TABLE  *SystemTable
  )
{

  gIH = ImageHandle;
  gST = SystemTable;
  gBS = SystemTable->BootServices;

  // プロトコルを作る
  HELLO_WORLD_PROTOCOL *HelloWorld;
  gBS->AllocatePool(EfiLoaderData, sizeof(HELLO_WORLD_PROTOCOL), &HelloWorld);
  HelloWorld->HelloWorld = MyHelloWorld;

  // インストール
  EFI_HANDLE HelloHandle = NULL;
  gBS->InstallMultipleProtocolInterfaces(
    &HelloHandle,
    &gHelloWorldProtocolGuid, HelloWorld
    );

  // 使ってみる
  HELLO_WORLD_PROTOCOL *Hello;
  gBS->LocateProtocol(&gHelloWorldProtocolGuid, NULL, &Hello);
  Hello->HelloWorld();

  return EFI_SUCCESS;
}
```

実際に実行すると、Hello Worldが表示できていることが確認できます。

![/img/post/2017-07-07-install-protocol.png](/img/post/2017-07-07-install-protocol.png)
