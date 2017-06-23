---
layout: post
title: メモリ上の.efiイメージをロードして実行する
tags:
- UEFI
- C
---

`EFI_BOOT_SERVICES.LoadImage()`は、メモリ上の.efiイメージも読み込むことができます。

*LoadMem.efi*

``` c
#include <Uefi.h>
#include <Library/UefiApplicationEntryPoint.h>
#include <Library/UefiLib.h>
#include <Library/DevicePathLib.h>
#include <Protocol/SimpleFileSystem.h>
#include <Guid/FileInfo.h>

EFI_HANDLE gIH;
EFI_SYSTEM_TABLE *gST;
EFI_BOOT_SERVICES *gBS;

#define DEFAULT_BUF_SIZE 512

EFI_DEVICE_PATH_PROTOCOL
*CreateMemoryMappedPath (
  VOID *StartAddr,
  VOID *EndAddr
  ) {
  MEMMAP_DEVICE_PATH *ImgPath;
  gBS->AllocatePool(EfiLoaderData, sizeof(MEMMAP_DEVICE_PATH) * 2, &ImgPath);

  ImgPath[0].Header.Type = HARDWARE_DEVICE_PATH;
  ImgPath[0].Header.SubType = HW_MEMMAP_DP;
  ImgPath[0].Header.Length[0] = (UINT8)sizeof(MEMMAP_DEVICE_PATH);
  ImgPath[0].Header.Length[1] = (UINT8)(sizeof(MEMMAP_DEVICE_PATH) >> 8);
  ImgPath[0].StartingAddress = (EFI_PHYSICAL_ADDRESS)StartAddr;
  ImgPath[0].EndingAddress = (EFI_PHYSICAL_ADDRESS)EndAddr;
  ImgPath[1].Header.Type = END_DEVICE_PATH_TYPE;
  ImgPath[1].Header.SubType = END_ENTIRE_DEVICE_PATH_SUBTYPE;
  ImgPath[1].Header.Length[0] = (UINT8)sizeof(EFI_DEVICE_PATH);
  ImgPath[1].Header.Length[1] = (UINT8)(sizeof(EFI_DEVICE_PATH) >> 8);

  return (EFI_DEVICE_PATH_PROTOCOL *)ImgPath;
}

EFI_STATUS
EFIAPI
ExecuteEfiFile (
  CHAR16 *FilePath
  ) {
  EFI_STATUS Status;
  EFI_SIMPLE_FILE_SYSTEM_PROTOCOL *SimpleFileSystem;
  EFI_GUID SimpleFileSystemGuid = EFI_SIMPLE_FILE_SYSTEM_PROTOCOL_GUID;
  Status = gBS->LocateProtocol(
    &SimpleFileSystemGuid,
    NULL,
    &SimpleFileSystem
    );

  EFI_FILE_PROTOCOL *RootDir;
  SimpleFileSystem->OpenVolume(SimpleFileSystem, &RootDir);

  EFI_FILE_PROTOCOL *Executable;
  RootDir->Open(RootDir, &Executable, FilePath, EFI_FILE_MODE_READ, 0);

  EFI_GUID InfoType = EFI_FILE_INFO_ID;
  EFI_FILE_INFO *Info;
  UINTN InfoSize = DEFAULT_BUF_SIZE;
  while (TRUE) {
    gBS->AllocatePool(EfiLoaderData, InfoSize, &Info);
    Status = Executable->GetInfo(Executable, &InfoType, &InfoSize, Info);
    if (Status == EFI_BUFFER_TOO_SMALL) {
      // GetInfoはバッファが足りなくて失敗すると第三引数に必要な容量をセットしてくれる
      gBS->FreePool(Info);
      continue;
    }
    break;
  }

  UINTN FileSize = (UINTN)Info->FileSize;
  CHAR8 *Buf;
  gBS->AllocatePool(EfiLoaderData, FileSize, &Buf);
  Executable->Read(Executable, &FileSize, Buf);

  EFI_DEVICE_PATH_PROTOCOL *MemPath = CreateMemoryMappedPath(Buf, Buf + FileSize - 1);

  EFI_HANDLE ChildHandle;
  Status = gBS->LoadImage(
    FALSE,
    gIH,
    MemPath,
    Buf,
    FileSize,
    &ChildHandle
    );

  UINTN ExitDataSize;
  CHAR16 *ExitData;
  gBS->StartImage(ChildHandle, &ExitDataSize, &ExitData);
  gBS->UnloadImage(ChildHandle);

  gBS->FreePool(Buf);
  gBS->FreePool(Info);
  gBS->FreePool(MemPath);
  RootDir->Close(RootDir);
  Executable->Close(Executable);

  return EFI_SUCCESS;
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

  ExecuteEfiFile(L"\\MyHelloWorld.efi");

  return EFI_SUCCESS;
}
```

[前回](/2017/06/23/01-load-image.html)みたいに`LocateHandleBuffer`して`while`で回したりはしていません。若干手抜きです。

少し難しいのがDevice Pathの指定の方法です。`OpenProtocol`とか`DevicePathFromHandle`とかで取得する分には何も考えなくていいのですが、今回はメモリ上の位置を表すDevice Pathを自分で作ってあげなければいけません。

Device Pathは`EFI_DEVICE_PATH_PROTOCOL`かもしくはその派生型の配列となっており、文字列がNULL終端となっているのと似たような感じで、最後の要素にはDevice Pathの終端を表す特殊な要素をおいておきます。今回はメモリ上なので、`MEMMAP_DEVICE_PATH`を使用しています。

もう一つ重要なのは`EFI_FILE_PROTOCOL.GetInfo`の使い方です。`GetInfo`によって取得できる`EFI_FILE_INFO`の容量は可変なので、呼び出し前に確保しておくべき容量がわかりません。そこで、最初は適当に容量をとって、`GetInfo`に失敗したらやり直すみたいなやり方をしています。

読み込むファイルは前回と同じです。

*MyHelloWorld.c*

``` c
#include <Uefi.h>
#include <Library/UefiApplicationEntryPoint.h>
#include <Library/UefiLib.h>


EFI_STATUS
EFIAPI
UefiMain (
  IN EFI_HANDLE        ImageHandle,
  IN EFI_SYSTEM_TABLE  *SystemTable
  )
{
  Print(L"Helo, World\n");
  return EFI_SUCCESS;
}
```

実行結果:

![/img/post/2017-06-23-load-from-mem.png](/img/post/2017-06-23-load-from-mem.png)