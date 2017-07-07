---
layout: post
title: プロトコルがインストールされたことを通知する
tags:
- UEFI
- C
---

`RegisterProtocolNotify`を使うと、指定したプロトコルが新たにインストールされた時に通知できます。

インストールするプロトコルは[前のやつ](/2017/07/07/03-uefi-create-protocol.html)と同じ、`HELLO_WORLD_PROTOCOL`を使います。

``` c
#define HELLO_WORLD_PROTOCOL_GUID \
  { 0x1020e163, 0x691d, 0x4202, { 0xa7, 0x76, 0x70, 0x73, 0x64, 0x36, 0x23, 0xbd } }

EFI_GUID gHelloWorldProtocolGuid = HELLO_WORLD_PROTOCOL_GUID;

typedef EFI_STATUS (EFIAPI *HELLO_WORLD)();

typedef struct HELLO_WORLD_PROTOCOL {
  HELLO_WORLD HelloWorld;
} HELLO_WORLD_PROTOCOL;


EFI_STATUS
EFIAPI
MyHelloWorld (
  IN HELLO_WORLD_PROTOCOL *This
  )
{
  return gST->ConOut->OutputString(gST->ConOut, L"Hello, world!\r\n");
}
```

プロトコルのインストールの通知には、[例によって`Event`を使います。](/2017/07/03/01-uefi-event.html)

`CreateEvent()`で作った`Event`を`RegisterProtocolNotify()`に渡すことによって、指定されたGUIDのプロトコルがインストールされた時にイベントを発火させるようにすることができます。

``` c
#include <Uefi.h>
#include <Library/UefiApplicationEntryPoint.h>
#include <Library/UefiLib.h>

EFI_HANDLE gIH;
EFI_SYSTEM_TABLE *gST;
EFI_BOOT_SERVICES *gBS;

void
EFIAPI
NotifyInstalled (
  IN EFI_EVENT ProtocolInstalled,
  IN VOID *Context
  )
{
  Print(L"protocol installed\n");
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

  VOID *Registration;
  EFI_EVENT ProtocolInstalled;
  gBS->CreateEvent(
    EVT_NOTIFY_SIGNAL,
    TPL_CALLBACK,
    NotifyInstalled,
    &Registration,
    &ProtocolInstalled);

  gBS->RegisterProtocolNotify(&gHelloWorldProtocolGuid, ProtocolInstalled, &Registration);

  HELLO_WORLD_PROTOCOL *HelloWorld;
  gBS->AllocatePool(EfiLoaderData, sizeof(HELLO_WORLD_PROTOCOL), &HelloWorld);
  HelloWorld->HelloWorld = MyHelloWorld;
  EFI_HANDLE HelloHandle = NULL;
  gBS->InstallMultipleProtocolInterfaces(
    &HelloHandle,
    &gHelloWorldProtocolGuid, HelloWorld
    );

  return EFI_SUCCESS;
}
```

実行結果:

![/img/post/2017-07-07-register-protocol-notify.png](/img/post/2017-07-07-register-protocol-notify.png)
