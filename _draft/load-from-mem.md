LocateHandleBufferしてwhileで回すのがめんどいので前のやつより手抜き

EFI_DEVICE_PATH_PROTOCOLはベースとなる型
それらのしたにいくつかフィールドを追加した型が定義されており、通常はそれらを使う
メモリ上ならMEMMAP_DEVICE_PATH
構造体のサイズはEFI_DEIVCE_PATH_PROTOCOL.Lengthで指定
Device PathはEFI_DEVICE_PATH_PROTOCOLの配列で表される
EFI_DEVICE_PATH_PROTOCOL DevPath[] = { HOGE, FUGA, FOO, END } みたいになってたら
\hoge\fuga\foo を表す
末尾はパスの終端を表す特殊な値。文字列でいう\0みたいなやつ

あと重要なのはGetInfo
必要な容量が最初はわからないので、最初は適当に容量をとってGetInfoに失敗したらやり直すみたいなのがかしこい？
GetInfoはバッファが足りなくて失敗すると第三引数に必要な容量をセットしてくれる

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

