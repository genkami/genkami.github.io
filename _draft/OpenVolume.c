/** @file
  Brief Description of UEFI MyHelloWorld
  Detailed Description of UEFI MyHelloWorld
  Copyright for UEFI MyHelloWorld
  License for UEFI MyHelloWorld
**/

#include <Uefi.h>
#include <Library/UefiApplicationEntryPoint.h>
#include <Library/UefiLib.h>
#include <Library/DevicePathLib.h>
#include <Protocol/SimpleFileSystem.h>
#include <Guid/FileInfo.h>

#define REPORT_ERROR_AND_DIE(Status) \
  { \
    ReportError(Status); \
    return Status; \
  }

void ReportError(EFI_STATUS stat) {
  if (stat == EFI_SUCCESS) {
    Print(L"Success\n");
  } else if (stat == EFI_BUFFER_TOO_SMALL) {
    Print(L"Buffer Too Small\n");
  } else if (stat == EFI_DEVICE_ERROR) {
    Print(L"Device Error\n");
  } else if (stat == EFI_INVALID_PARAMETER) {
    Print(L"Invalid Parameter\n");
  } else if (stat == EFI_LOAD_ERROR) {
    Print(L"Load Error\n");
  } else if (stat == EFI_MEDIA_CHANGED) {
    Print(L"Media Changed\n");
  } else if (stat == EFI_NO_MEDIA) {
    Print(L"No Media\n");
  } else if (stat == EFI_NOT_FOUND) {
    Print(L"Not Found\n");
  } else if (stat == EFI_SECURITY_VIOLATION) {
    Print(L"Security VIolation\n");
  } else if (stat == EFI_UNSUPPORTED) {
    Print(L"Unsupported\n");
  } else if (stat == EFI_VOLUME_CORRUPTED) {
    Print(L"Volume Corrupted\n");
  } else {
    Print(L"Unknown Error\n");
  }
}


/**
  as the real entry point for the application.

  @param[in] ImageHandle    The firmware allocated handle for the EFI image.  
  @param[in] SystemTable    A pointer to the EFI System Table.
  
  @retval EFI_SUCCESS       The entry point is executed successfully.
  @retval other             Some error occurs when executing this entry point.

**/
EFI_STATUS
EFIAPI
UefiMain (
  IN EFI_HANDLE        ImageHandle,
  IN EFI_SYSTEM_TABLE  *SystemTable
  )
{
  EFI_STATUS Status;
  EFI_SIMPLE_FILE_SYSTEM_PROTOCOL *SimpleFileSystem;
  EFI_GUID SimpleFileSystemGuid = EFI_SIMPLE_FILE_SYSTEM_PROTOCOL_GUID;
  Status = SystemTable->BootServices->LocateProtocol(
    &SimpleFileSystemGuid,
    NULL,
    &SimpleFileSystem
    );
  if (Status == EFI_SUCCESS) Print(L"Simple File System Located\n");
  else REPORT_ERROR_AND_DIE(Status);

  EFI_FILE_PROTOCOL *Root;
  Status = SimpleFileSystem->OpenVolume(SimpleFileSystem, &Root);
  if (Status == EFI_SUCCESS) Print(L"Open Root\n");
  else REPORT_ERROR_AND_DIE(Status);

  EFI_FILE_PROTOCOL *Executable;
  Status = Root->Open(Root, &Executable, L"MyHelloWorld.efi", EFI_FILE_MODE_READ, 0);
  if (Status == EFI_SUCCESS) Print(L"Open .efi file\n");
  else REPORT_ERROR_AND_DIE(Status);

  EFI_GUID FileInfoGuid = EFI_FILE_INFO_ID;
  EFI_FILE_INFO *FileInfo;
  UINTN InfoSize = 50 * 1000 * 1000;

  Status = SystemTable->BootServices->AllocatePool(
    EfiBootServicesData, InfoSize, &FileInfo);
  if (Status == EFI_SUCCESS) Print(L"Memory Allocated\n");
  else REPORT_ERROR_AND_DIE(Status);

  Status = Executable->GetInfo(Executable, &FileInfoGuid, &InfoSize, FileInfo);
  if (Status == EFI_SUCCESS) Print(L"Get Info\n");
  else {
    Print(L"At least %d bytes are required to store Info\n");
    REPORT_ERROR_AND_DIE(Status);
  }

  UINTN FileSize = (UINTN)(FileInfo->FileSize);
  Status = SystemTable->BootServices->FreePool(FileInfo);
  if (Status == EFI_SUCCESS) Print(L"Free\n");

  VOID *Buffer;
  Status = SystemTable->BootServices->AllocatePool(
    EfiBootServicesCode, FileSize, &Buffer);
  if (Status == EFI_SUCCESS) Print(L"Buffer Allocated (%d bytes)\n", FileSize);
  else REPORT_ERROR_AND_DIE(Status);

  Status = Executable->Read(Executable, &FileSize, Buffer);
  if (Status == EFI_SUCCESS) Print(L"Read Executable\n");
  else REPORT_ERROR_AND_DIE(Status);

  MEMMAP_DEVICE_PATH ImgPath[2];
  ImgPath[0].Header.Type = HARDWARE_DEVICE_PATH;
  ImgPath[0].Header.SubType = HW_MEMMAP_DP;
  ImgPath[0].Header.Length[0] = (UINT8)sizeof(MEMMAP_DEVICE_PATH);
  ImgPath[0].Header.Length[1] = (UINT8)(sizeof(MEMMAP_DEVICE_PATH) >> 8);
  ImgPath[0].StartingAddress = (EFI_PHYSICAL_ADDRESS)Buffer;
  ImgPath[0].EndingAddress = (EFI_PHYSICAL_ADDRESS)((UINT8 *)Buffer + FileSize);
  ImgPath[1].Header.Type = END_DEVICE_PATH_TYPE;
  ImgPath[1].Header.SubType = END_ENTIRE_DEVICE_PATH_SUBTYPE;
  ImgPath[1].Header.Length[0] = (UINT8)sizeof(EFI_DEVICE_PATH);
  ImgPath[1].Header.Length[1] = (UINT8)(sizeof(EFI_DEVICE_PATH) >> 8);
  Print(L"Set Device Path\n");

  EFI_HANDLE ExecutableHandle;
  Status = SystemTable->BootServices->LoadImage(
    0,
    ImageHandle,
    (EFI_DEVICE_PATH *)ImgPath,
    Buffer,
    FileSize,
    &ExecutableHandle);
  if (Status == EFI_SUCCESS) Print(L"Load Image\n");
  else REPORT_ERROR_AND_DIE(Status);

  UINTN ExitDataSize;
  CHAR16 *ExitData;
  Status = SystemTable->BootServices->StartImage(ExecutableHandle, &ExitDataSize, &ExitData);
  if (Status == EFI_SUCCESS) {
    Print(L"Execution Finished\n");
    if (ExitDataSize > 0) Print(L"ExitData: %s\n", ExitData);
  } else REPORT_ERROR_AND_DIE(Status);

  Print(L"Done.\n"); 
  return EFI_SUCCESS;
}