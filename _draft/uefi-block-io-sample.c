/** @file
  Brief Description of UEFI MyHelloWorld
  Detailed Description of UEFI MyHelloWorld
  Copyright for UEFI MyHelloWorld
  License for UEFI MyHelloWorld
**/

#include <Uefi.h>
#include <Base.h>
#include <Protocol/BlockIo.h>
#include <Protocol/DevicePath.h>
#include <Protocol/LoadedImage.h>
#include <Library/UefiApplicationEntryPoint.h>
#include <Library/UefiLib.h>
#include <Library/DevicePathLib.h>

//#define EFI_BLOCK_IO_PROTOCOL_GUID {0x964e5b21,0x6459,0x11d2,0x8e,0x39,0x00,0xa0,0xc9,0x69,0x72,0x3b}
//static EFI_GUID gEfiBlockIoProtocolGuid = EFI_BLOCK_IO_PROTOCOL_GUID;

EFI_STATUS Status;
EFI_SYSTEM_TABLE *gST;
EFI_BOOT_SERVICES *gBS;
EFI_RUNTIME_SERVICES *gRT;

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
  IN EFI_HANDLE        mBdsImageHandle,
  IN EFI_SYSTEM_TABLE  *SystemTable
  )
{
    UINTN NumberFileSystemHandles;
    EFI_HANDLE *FileSystemHandles;
    UINTN Index;
    EFI_BLOCK_IO* BlkIo;
    EFI_DEVICE_PATH_PROTOCOL *FilePath;
    EFI_HANDLE ImageHandle = NULL;
    EFI_LOADED_IMAGE_PROTOCOL* ImageInfo;

    gST = SystemTable;
    gBS = gST -> BootServices;
    gRT = gST -> RuntimeServices;

    gBS->LocateHandleBuffer(ByProtocol,
                            &gEfiBlockIoProtocolGuid,
                            NULL,
                            &NumberFileSystemHandles,
                            &FileSystemHandles); 

    Print(L"after LocateHandleBuffer \n"); 
    for(Index = 0; Index<NumberFileSystemHandles; ++Index) {
        Status = gBS -> HandleProtocol(FileSystemHandles[Index], 
                                        &gEfiBlockIoProtocolGuid,
                                        (VOID**) &BlkIo);

        Print(L"after HandleProtocol %x - %r\n",FileSystemHandles[Index], Status); 
        if(!EFI_ERROR(Status)) {
        Print(L"after EFI_ERROR(Status) \n"); 
            if(!BlkIo->Media->RemovableMedia || BlkIo->Media->RemovableMedia) {
                FilePath = FileDevicePath(FileSystemHandles[Index],
                                            L"\\project1\\MyHelloWorld.efi");
                Print(L"after FileDevicePath - %x - %x\n",FilePath->Type,FilePath->SubType); 
                Status = gBS -> LoadImage(FALSE, mBdsImageHandle, FilePath, NULL, 0, &ImageHandle);
                Print(L"after LoadImage - %d - %d - %r \n", ImageHandle, EFI_ERROR(Status), Status); 
                if(!EFI_ERROR(Status)) {
                    Status = gBS -> HandleProtocol(ImageHandle, &gEfiLoadedImageProtocolGuid, 
                                                    (VOID **) &ImageInfo);
                    Print(L"after HandleProtocol \n"); 
                    if(!EFI_ERROR(Status)) {
                        if(ImageInfo->ImageCodeType == EfiLoaderCode) {
                            gBS->FreePool(FilePath);
                        }
                    }
                }
                Status = gBS -> StartImage(ImageHandle, NULL, NULL);
                Print(L"after StartImage - %d\n" , EFI_ERROR(Status)); 
                continue;
            }
        }       
    }
  return EFI_SUCCESS;
}