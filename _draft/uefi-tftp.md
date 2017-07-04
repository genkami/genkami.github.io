---
layout: post
title: 
tags:
- 
---

``` sh
$ cat /path/to/tftp_root/hello.txt
hello world
```

``` c
#include <Uefi.h>
#include <Library/UefiApplicationEntryPoint.h>
#include <Library/UefiLib.h>
#include <Library/PrintLib.h>
#include <Protocol/ServiceBinding.h>
#include <Protocol/Dhcp4.h>
#include <Protocol/Mtftp4.h>

EFI_HANDLE gIH;
EFI_SYSTEM_TABLE *gST;
EFI_BOOT_SERVICES *gBS;

EFI_GUID gEfiDhcp4ProtocolGuid = EFI_DHCP4_PROTOCOL_GUID;
EFI_GUID gEfiDhcp4ServiceBindingProtocolGuid = EFI_DHCP4_SERVICE_BINDING_PROTOCOL_GUID;
EFI_GUID gEfiMtftp4ProtocolGuid = EFI_MTFTP4_PROTOCOL_GUID;
EFI_GUID gEfiMtftp4ServiceBindingProtocolGuid = EFI_MTFTP4_SERVICE_BINDING_PROTOCOL_GUID;

#define MTFTP_BUF_SIZE 128

EFI_EVENT MtftpConnectionDone;

EFI_STATUS
EFIAPI
LocateProtocolByServiceBindingProtocol (
  IN EFI_GUID *ServiceBindingProtocolGuid,
  IN EFI_GUID *ProtocolGuid,
  OUT EFI_SERVICE_BINDING_PROTOCOL **ServiceBinding,
  OUT VOID **Interface,
  OUT EFI_HANDLE *Handle
  )
{
  EFI_STATUS Status;
  EFI_HANDLE *Handles;
  UINTN NoHandles;
  Status = gBS->LocateHandleBuffer(
    ByProtocol,
    ServiceBindingProtocolGuid,
    NULL,
    &NoHandles,
    &Handles
    );
  if (Status != EFI_SUCCESS) {
    return Status;
  }

  UINTN ProtocolFound = FALSE;
  for (UINTN i = 0; i < NoHandles; i++) {
    *Handle = Handles[i];
    Status = gBS->OpenProtocol(
      *Handle,
      ServiceBindingProtocolGuid,
      ServiceBinding,
      gIH,
      NULL,
      EFI_OPEN_PROTOCOL_GET_PROTOCOL
      );
    if (Status != EFI_SUCCESS) {
      continue;
    }

    Status = (*ServiceBinding)->CreateChild(*ServiceBinding, Handle);
    if (Status != EFI_SUCCESS) {
      gBS->CloseProtocol(*Handle, ServiceBindingProtocolGuid, gIH, NULL);
      continue;
    }

    Status = gBS->OpenProtocol(
      *Handle,
      ProtocolGuid,
      Interface,
      gIH,
      NULL,
      EFI_OPEN_PROTOCOL_GET_PROTOCOL
      );
    if (Status != EFI_SUCCESS) {
      (*ServiceBinding)->DestroyChild(*ServiceBinding, *Handle);
      gBS->CloseProtocol(*Handle, ServiceBindingProtocolGuid, gIH, NULL);
      continue;
    }

    ProtocolFound = TRUE;
    break;
  }

  if (!ProtocolFound) {
    return EFI_NOT_FOUND;
  }

  gBS->FreePool(Handles);

  return EFI_SUCCESS;
}

EFI_STATUS
EFIAPI
MtftpCheckPacket (
  IN EFI_MTFTP4_PROTOCOL *This,
  IN EFI_MTFTP4_TOKEN *Token,
  IN UINT16 PacketLen,
  IN EFI_MTFTP4_PACKET *Packet
  )
{
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

  EFI_HANDLE Dhcp4Handle;
  EFI_SERVICE_BINDING_PROTOCOL *Dhcp4Binding;
  EFI_DHCP4_PROTOCOL *Dhcp4;
  LocateProtocolByServiceBindingProtocol(
    &gEfiDhcp4ServiceBindingProtocolGuid,
    &gEfiDhcp4ProtocolGuid,
    &Dhcp4Binding,
    &Dhcp4,
    &Dhcp4Handle
    );

  // start dhcp4 configuration process
  EFI_DHCP4_CONFIG_DATA Dhcp4Config = {
    .DiscoverTryCount = 0,
    .DiscoverTimeout = NULL,
    .RequestTryCount = 0,
    .RequestTimeout = NULL,
    .ClientAddress = { 0, 0, 0, 0 },
    .Dhcp4Callback = NULL,
    .CallbackContext = NULL,
    .OptionCount = 0,
    .OptionList = NULL
  };
  Dhcp4->Configure(Dhcp4, &Dhcp4Config);
  Dhcp4->Start(Dhcp4, NULL);

  EFI_DHCP4_MODE_DATA ModeData;
  Dhcp4->GetModeData(Dhcp4, &ModeData);


  Print(L"ipv4 address:\n");
  Print(
    L"%d.%d.%d.%d\n",
    ModeData.ClientAddress.Addr[0],
    ModeData.ClientAddress.Addr[1],
    ModeData.ClientAddress.Addr[2],
    ModeData.ClientAddress.Addr[3]
    );

  /*
   * ここからTFTPの処理
   */

  EFI_HANDLE Mtftp4Handle;
  EFI_SERVICE_BINDING_PROTOCOL *Mtftp4Binding;
  EFI_MTFTP4_PROTOCOL *Mtftp4;
  LocateProtocolByServiceBindingProtocol(
    &gEfiMtftp4ServiceBindingProtocolGuid,
    &gEfiMtftp4ProtocolGuid,
    &Mtftp4Binding,
    &Mtftp4,
    &Mtftp4Handle
    );

  // an event that is signaled when tftp connection is done
  gBS->CreateEvent(
    0,
    TPL_CALLBACK,
    NULL,
    NULL,
    &MtftpConnectionDone
    );

  EFI_IPv4_ADDRESS ServerAddress = SERVER_ADDRESS;

  EFI_MTFTP4_CONFIG_DATA Mtftp4Config;
  Mtftp4Config.UseDefaultSetting = FALSE;
  Mtftp4Config.StationIp = ModeData.ClientAddress;
  Mtftp4Config.SubnetMask = ModeData.SubnetMask;
  Mtftp4Config.LocalPort = 1234; // クライアントのポートは適当に
  Mtftp4Config.GatewayIp = ModeData.RouterAddress;
  Mtftp4Config.ServerIp = ServerAddress;
  Mtftp4Config.InitialServerPort = 69;
  Mtftp4Config.TryCount = 5;
  Mtftp4Config.TimeoutValue = 30;

  Mtftp4->Configure(Mtftp4, &Mtftp4Config);

  CHAR8 *FileBuffer;
  gBS->AllocatePool(
    EfiBootServicesData,
    MTFTP_BUF_SIZE,
    &FileBuffer
    );

  CHAR16 *UnicodeBuffer;
  gBS->AllocatePool(
    EfiBootServicesData,
    MTFTP_BUF_SIZE * 2,
    &UnicodeBuffer
    );
 

  EFI_MTFTP4_TOKEN ReadFileToken;
  ReadFileToken.Event = MtftpConnectionDone;
  ReadFileToken.OverrideData = NULL;
  ReadFileToken.ModeStr = "netascii"; // "netascii" or "octet"
  ReadFileToken.Filename = "hello.txt";
  ReadFileToken.OptionCount = 0;
  ReadFileToken.OptionList = NULL;
  ReadFileToken.BufferSize = MTFTP_BUF_SIZE - 1; // we will add '\0' to the end of content
  ReadFileToken.Buffer = FileBuffer;
  ReadFileToken.Context = NULL;
  ReadFileToken.CheckPacket = MtftpCheckPacket;
  ReadFileToken.TimeoutCallback = NULL;
  ReadFileToken.PacketNeeded = NULL;

  Mtftp4->ReadFile(Mtftp4, &ReadFileToken);

  UINTN EventIndex;
  gBS->WaitForEvent(1, &MtftpConnectionDone, &EventIndex);

  if (ReadFileToken.Status != EFI_SUCCESS) {
    Print(L"an error occurred during mtftp connection\n");
    return ReadFileToken.Status;
  }

  FileBuffer[ReadFileToken.BufferSize] = L'\0';
  UnicodeSPrintAsciiFormat(UnicodeBuffer, MTFTP_BUF_SIZE * 2, FileBuffer);

  Print(L"content:\n");
  Print(L"%s", UnicodeBuffer);

  gBS->FreePool(FileBuffer);
  gBS->FreePool(UnicodeBuffer);
  gBS->CloseEvent(MtftpConnectionDone);
  Dhcp4->Release(Dhcp4);
  Mtftp4Binding->DestroyChild(Mtftp4Binding, Mtftp4Handle);
  Dhcp4Binding->DestroyChild(Dhcp4Binding, Dhcp4Handle);
  Print(L"Done.\n"); 
  return EFI_SUCCESS;
}
```

通信が終わった後のイベントを作る
イベントのコンテキストも作る
タイムアウト時のコールバック関数を作る
パケットのチェック用関数を作る

`OverrideData`を指定すると、`Configure()`でした設定を上書きできる。

``` c
typedef struct {
  EFI_IPv4_ADDRESS GatewayIp;
  EFI_IPv4_ADDRESS ServerIp;
  UINT16 ServerPort;
  UINT16 TryCount;
  UINT16 TimeoutValue;
} EFI_MTFTP4_OVERRIDE_DATA;
```

Token

``` c
typedef struct {
  EFI_STATUS Status;
  EFI_EVENT Event;
  EFI_MTFTP4_OVERRIDE_DATA *OverrideData;
  UINT8 *Filename;
  UINT8 *ModeStr;
  UINT32 OptionCount;
  EFI_MTFTP4_OPTION *OptionList;
  UINT64 BufferSize;
  VOID *Buffer;
  VOID *Context;
  EFI_MTFTP4_CHECK_PACKET CheckPacket;
  EFI_MTFTP4_TIMEOUT_CALLBACK TimeoutCallback;
  EFI_MTFTP4_PACKET_NEEDED PacketNeeded;
} EFI_MTFTP4_TOKEN;
```

CheckPacket

``` c
typedef
EFI_STATUS
(EFIAPI *EFI_MTFTP4_CHECK_PACKET) (
  IN EFI_MTFTP4_PROTOCOL *This,
  IN EFI_MTFTP4_TOKEN *Token,
  IN UINT16 PacketLen,
  IN EFI_MTFTP4_PACKET *Packet
);
```

TimeoutCallback

``` c
typedef
EFI_STATUS
(EFIAPI *EFI_MTFTP4_TIMEOUT_CALLBACK) (
  IN EFI_MTFTP4_PROTOCOL *This,
  IN EFI_MTFTP4_TOKEN *Token
);
```

接続がタイムアウトしたときに呼ばれる関数。EFI_SUCCEESS以外を返すと`ReadFile()`の処理は中断される。

PacketNeeded

``` c
typedef
EFI_STATUS
(EFIAPI *EFI_MTFTP4_PACKET_NEEDED) (
  IN EFI_MTFTP4_PROTOCOL *This,
  IN EFI_MTFTP4_TOKEN *Token,
  IN OUT UINT16 *Length,
  OUT VOID **Buffer
);
```

送信用のパケットを生成する関数。`ReadFile`では使用しないが、`WriteFile`では`Buffer`に送信データを用意する代わりに、`PacketNeeded`を呼び出してパケットを直接作ることもできる。
`PacketNeeded`はパケットの内容を`Buffer`に、その長さを`Length`にセットする。`Length`を`0`にセットすることで終了を表す。
