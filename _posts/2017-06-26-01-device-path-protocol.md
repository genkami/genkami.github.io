---
layout: post
title: 特定のプロトコルをサポートするハンドラをリストアップする
tags:
- UEFI
---
UEFIのプロトコルを使うときは、基本的に`LocateHandleBuffer`でプロトコルをサポートするハンドラをみつけて、`OpenProtocol`していくのが一般的なやり方のようです。

``` c
#include <Uefi.h>
#include <Library/UefiApplicationEntryPoint.h>
#include <Library/UefiLib.h>
#include <Library/DevicePathLib.h>

EFI_HANDLE gIH;
EFI_SYSTEM_TABLE *gST;
EFI_BOOT_SERVICES *gBS;

EFI_STATUS PrintAllDevicePaths () {
  EFI_STATUS Status;
  EFI_GUID DevicePathGuid = EFI_DEVICE_PATH_PROTOCOL_GUID;
  EFI_HANDLE *Handles;
  UINTN NoHandles;
  Status = gBS->LocateHandleBuffer
    (
     ByProtocol,
     &DevicePathGuid,
     NULL,
     &NoHandles,
     &Handles
     );
  if (Status != EFI_SUCCESS) {
    Print(L"failed to locate EFI_DEVICE_PATH_PROTOCOL\n");
    return Status;
  }

  for (UINTN i = 0; i < NoHandles; i++) {
    EFI_DEVICE_PATH_PROTOCOL *DevicePath;
    Status = gBS->OpenProtocol
      (
       Handles[i],
       &DevicePathGuid,
       &DevicePath,
       gIH,
       NULL,
       EFI_OPEN_PROTOCOL_GET_PROTOCOL
       );
    if (Status != EFI_SUCCESS) {
      Print(L"can't open device path protocol\n");
      return Status;
    }
    CHAR16 *Buf = ConvertDevicePathToText(DevicePath, TRUE, TRUE);
    Print(L"%s\n", Buf);
    gBS->FreePool(Buf);
  }
  return EFI_SUCCESS;
}


EFI_STATUS
EFIAPI
UefiMain
(
 IN EFI_HANDLE ImageHandle,
 IN EFI_SYSTEM_TABLE *SystemTable
 ) {
  EFI_STATUS Status;

  gIH = ImageHandle;
  gST = SystemTable;
  gBS = SystemTable->BootServices;

  Status = PrintAllDevicePaths();
  if (Status != EFI_SUCCESS) {
    Print(L"error: %d\n", Status);
  }

  return EFI_SUCCESS;
}
```

この例では、`EFI_DEVICE_PATH_PROTOCOL`をサポートしているハンドラを`LocateHandleBuffer`で見つけ出して、それらの`Device Path`の文字列による表現を表示しています。

正常に実行されれば以下のようは表示がされるはずです。

![/img/post/2017-06-26-device-path.png](/img/post/2017-06-26-device-path.png)
