---
layout: post
title: UEFIでブロックデバイスにアクセスする
tags:
- C
- UEFI
---

`EFI_BLOCK_IO_PROTOCOL`を使うと、ハードディスクなどのブロックデバイスの読み書きができます。

ただ読むだけでは面白くないので、GPTパーティションのヘッダを読んで見ることにしました。

[http://www.ntfs.com/guid-part-table.htm](http://www.ntfs.com/guid-part-table.htm)

上のサイトをによると、GTPのヘッダの情報はハードディスクの1番目のLBA の先頭から順に以下のように並んでいるようです。

``` c
typedef struct GPT_HEADER {
  UINT64 Signature;           // GPTのシグネチャ。 0x5452415020494645
  UINT32 Revision;            // リビジョン
  UINT32 Size;                // ヘッダのサイズ
  CHAR8 CRC32Header[4];       // ヘッダ全体のCRC32
  CHAR8 Reserved[4];          // 予約領域
  UINT64 CurrentLBA;          // ヘッダのLBA
  UINT64 BackupLBA;           // バックアップのLBA
  UINT64 UsableLBAStart;      // パーティションに使えるLBAの開始位置
  UINT64 UsableLBAEnd;        // ↑の終了位置
  CHAR8 Guid[16];             // ハードディスクのGUID
  UINT64 PartitionLBAStart;   // パーティションエントリの開始位置
  UINT32 NumPartitions;       // パーティションエントリの数
  UINT32 PartitionEntrySize;  // パーティションエントリのサイズ
  CHAR8 CRC32Partition[4];    // パーティションエントリのCRC32
} GPT_HEADER;
```

このデータを実際にハードディスクの頭から取ってきます。


``` c
#include <Uefi.h>
#include <Library/UefiApplicationEntryPoint.h>
#include <Library/UefiLib.h>
#include <Library/DevicePathLib.h>
#include <Protocol/BlockIo.h>

EFI_GUID gEfiBlockIoProtocolGuid = EFI_BLOCK_IO_PROTOCOL_GUID;

EFI_HANDLE gIH;
EFI_SYSTEM_TABLE *gST;
EFI_BOOT_SERVICES *gBS;

#define GPT_HEADER_LBA_START 1

#define INPUT_BUF_SIZE 64

/*
 * ConIn から1行読む。↓のコピペ
 * https://genkami.github.io//2017/06/21/04-uefi-text-io.html
 */
EFI_STATUS
EFIAPI
GetLine (
  OUT CHAR16 *Buf,
  IN OUT UINTN *BufSize
  )
{
  EFI_STATUS Status;
  UINTN EventIndex;
  EFI_INPUT_KEY Key;
  for (UINTN i = 0; i < *BufSize; i++) {
    gBS->WaitForEvent(1, &gST->ConIn->WaitForKey, &EventIndex);
    Status = gST->ConIn->ReadKeyStroke(gST->ConIn, &Key);
    if (Status != EFI_SUCCESS) {
      return Status;
    }
    Buf[i] = Key.UnicodeChar;
    if (Buf[i] == L'\n' || Buf[i] == L'\r') {
      Print(L"\n");
      Buf[i] = L'\0';
      *BufSize = i;
      return EFI_SUCCESS;
    }
    Print(L"%c", Buf[i]);

  }
  return EFI_BUFFER_TOO_SMALL;
}

/*
 * 10進数の文字列を数値に変換(雑)
 */
EFI_STATUS
EFIAPI
ParseInt (
  IN CHAR16 *String,
  OUT UINTN *Number
  )
{
  if (*String == L'\0') return EFI_INVALID_PARAMETER;

  *Number = 0;
  for (; *String != L'\0'; String++) {
    *Number *= 10;
    if (L'0' <= *String && *String <= L'9') {
      *Number += *String - L'0';
    } else {
      return EFI_INVALID_PARAMETER;
    }
  }
  return EFI_SUCCESS;
}

/*
 * CHAR8を2桁の16進数で表示
 */
void
EFIAPI
PrintHex (
  IN CHAR8 Byte
  )
{
  for (UINTN i = 0; i < 2; i++) {
    CHAR16 Digit = (Byte >> 4) & 0xf;
    if (0x0 <= Digit && Digit < 0xA) {
      Print(L"%c", L'0' + Digit);
    } else {
      Print(L"%c", L'A' + Digit - 0xA);
    }
    Byte <<= 4;
  }
}

EFI_STATUS
EFIAPI
UefiMain (
  IN EFI_HANDLE ImageHandle,
  IN EFI_SYSTEM_TABLE *SystemTable
  )
{
  EFI_STATUS Status;

  UINTN NoHandles;
  EFI_HANDLE *Handles;
  Status = gBS->LocateHandleBuffer(
    ByProtocol,
    &gEfiBlockIoProtocolGuid,
    NULL,
    &NoHandles,
    &Handles
    );
  if (Status != EFI_SUCCESS) {
    Print(L"can't locate EFI_BLOCK_IO_PROTCOL\n");
    return Status;
  }

  /*
   * ブロックデバイスが多すぎてどれがどれかわからないので、Device Pathを表示して
   * それっぽいのを手動で選択する方式
   */
  Print(L"block io devices:\n");
  for (UINTN i = 0; i < NoHandles; i++) {
    EFI_DEVICE_PATH_PROTOCOL *Path = DevicePathFromHandle(Handles[i]);
    CHAR16 *PathText = ConvertDevicePathToText(Path, TRUE, TRUE);
    Print(L"[%d] %s\n", i, PathText);
    gBS->FreePool(PathText);
    gBS->FreePool(Path);
  }

  CHAR16 *Line;
  Status = gBS->AllocatePool(EfiLoaderData, INPUT_BUF_SIZE, &Line);
  if (Status != EFI_SUCCESS) {
    Print(L"can't allocate Line\n");
    return Status;
  }

  UINTN DeviceIndex;
  while (TRUE) {
    Print(L"which device to show GPT information?: ");

    UINTN LineSize = INPUT_BUF_SIZE;
    Status = GetLine(Line, &LineSize);
    if (Status != EFI_SUCCESS) continue; // 入力文字列が長すぎた?

    Status = ParseInt(Line, &DeviceIndex);
    if (Status != EFI_SUCCESS) {
      Print(L"please input number\n");
      continue;
    } else {
      break;
    }
  }

  /*
   * ここからが本番
   */

  EFI_BLOCK_IO_PROTOCOL *BlockIo;
  Status = gBS->OpenProtocol(
    Handles[DeviceIndex],
    &gEfiBlockIoProtocolGuid,
    &BlockIo,
    gIH,
    NULL,
    EFI_OPEN_PROTOCOL_GET_PROTOCOL
    );
  if (Status != EFI_SUCCESS) {
    Print(L"can't open EFI_BLOCK_IO_PROTCOL\n");
    return Status;
  }

  gBS->FreePool(Line);
  gBS->FreePool(Handles);

  // BlockIo->Mediaに、そのデバイスの情報が書かれている
  Print(L"removable media: %c\n", BlockIo->Media->RemovableMedia ? 'Y' : 'N');
  Print(L"logical partition: %c\n", BlockIo->Media->LogicalPartition ? 'Y' : 'N');

  UINT32 MediaId = BlockIo->Media->MediaId;
  UINTN BufferSize = BlockIo->Media->BlockSize;
  Print(L"logical block size: %d\n", BufferSize);

  Status = BlockIo->Reset(BlockIo, FALSE);
  if (Status != EFI_SUCCESS) {
    Print(L"EFI_BLOCK_IO_PROTCOL.Reset() failed\n");
    return Status;
  }

  GPT_HEADER *GptHeader;
  Status = gBS->AllocatePool(EfiLoaderData, BufferSize, &GptHeader);
  if (Status != EFI_SUCCESS) {
    Print(L"can't allocate GptHeader\n");
    return Status;
  }

  Status = BlockIo->ReadBlocks(
    BlockIo,
    MediaId,
    GPT_HEADER_LBA_START,
    BufferSize,
    GptHeader
    );
  if (Status != EFI_SUCCESS) {
    Print(L"EFI_BLOCK_IO_PROTCOL.ReadBlocks() failed\n");
    return Status;
  }

  Print(L"GPT header signature: %Lx\n", GptHeader->Signature);

  Print(L"Disk GUID: ");
  for (UINTN i = 0; i < 16; i++) {
    PrintHex(GptHeader->Guid[i]);
  }
  Print(L"\n");

  return EFI_SUCCESS;
}
```

実行結果がこちら

![/img/post/2017-07-04-read-gpt.png](/img/post/2017-07-04-read-gpt.png)

シグネチャが上のサイトに書かれている値 (= 0x5452415020494645) に一致しているので、正しく読めていそうです。

ちなみに、DiskPartでディスクの情報を調べてみた結果がこちら

![/img/post/2017-07-04-disk-info.png](/img/post/2017-07-04-disk-info.png)

GUIDの並びがずれていますが、多分エンディアンの違い的なアレでしょう。
