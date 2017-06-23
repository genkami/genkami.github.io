https://www.guidgen.com/


// OpenEfi.c
#include <Uefi.h>
#include <Library/UefiApplicationEntryPoint.h>
#include <Library/UefiLib.h>
#include <Library/DevicePathLib.h>
#include <Protocol/SimpleFileSystem.h>

EFI_HANDLE gIH;
EFI_SYSTEM_TABLE *gST;
EFI_BOOT_SERVICES *gBS;

EFI_STATUS
EFIAPI
ExecuteEfiFile (
  CHAR16 *FilePath
  ) {
  EFI_STATUS Status;

  EFI_GUID SimpleFileSystemGuid = EFI_SIMPLE_FILE_SYSTEM_PROTOCOL_GUID;
  EFI_HANDLE *Handles;
  UINTN NoHandles;
  gBS->LocateHandleBuffer(
    ByProtocol,
    &SimpleFileSystemGuid,
    NULL,
    &NoHandles,
    &Handles
    );

  for (UINTN i = 0; i < NoHandles; i++) {
    EFI_DEVICE_PATH_PROTOCOL *DevicePath = DevicePathFromHandle(Handles[i]);
    EFI_DEVICE_PATH_PROTOCOL *EfiPath = ConvertTextToDevicePath(FilePath);
    EFI_DEVICE_PATH_PROTOCOL *FullPath = AppendDevicePath(DevicePath, EfiPath);

    EFI_HANDLE ChildHandle;
    Status = gBS->LoadImage(
      FALSE,
      gIH,
      FullPath,
      NULL,
      0,
      &ChildHandle
      );
    if (Status != EFI_SUCCESS) {
      // maybe image not found
      goto LOOP_END;
    }

    UINTN ExitDataSize;
    CHAR16 *ExitData;
    gBS->StartImage(ChildHandle, &ExitDataSize, &ExitData);

    gBS->UnloadImage(ChildHandle);

    LOOP_END:
    gBS->FreePool(DevicePath);
    gBS->FreePool(EfiPath);
    gBS->FreePool(FullPath);
  }
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


// MyHelloWorld.c
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