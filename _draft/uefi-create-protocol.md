uefi-create-protocol

InstallMultipleProtocolInterfaces(&Handle, &guid1, &protocol1, &guid2, &protocol2, ...);

必要
プロトコルの型
実体
GUID
Handle

Handleにプロトコルをインストールする
HandleはNULLなら新しく作られる。


``` c
#include <Uefi.h>
#include <Library/UefiApplicationEntryPoint.h>
#include <Library/UefiLib.h>

EFI_HANDLE gIH;
EFI_SYSTEM_TABLE *gST;
EFI_BOOT_SERVICES *gBS;

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

  HELLO_WORLD_PROTOCOL *HelloWorld;
  gBS->AllocatePool(EfiLoaderData, sizeof(HELLO_WORLD_PROTOCOL), &HelloWorld);
  HelloWorld->HelloWorld = MyHelloWorld;
  EFI_HANDLE HelloHandle = NULL;
  gBS->InstallMultipleProtocolInterfaces(
    &HelloHandle,
    &gHelloWorldProtocolGuid, HelloWorld
    );

  HELLO_WORLD_PROTOCOL *Hello;
  gBS->LocateProtocol(&gHelloWorldProtocolGuid, NULL, &Hello);
  Hello->HelloWorld();

  return EFI_SUCCESS;
}
```