uefi-dhcp
EFI_DHCPv4_PROTOCOL or EFI_DHCPv6_PROTOCOL

``` c
#include <Uefi.h>
#include <Library/UefiApplicationEntryPoint.h>
#include <Library/UefiLib.h>
#include <Protocol/ServiceBinding.h>
#include <Protocol/Dhcp4.h>

EFI_HANDLE gIH;
EFI_SYSTEM_TABLE *gST;
EFI_BOOT_SERVICES *gBS;

EFI_GUID gEfiDhcp4ProtocolGuid = EFI_DHCP4_PROTOCOL_GUID;
EFI_GUID gEfiDhcp4ServiceBindingProtocolGuid = EFI_DHCP4_SERVICE_BINDING_PROTOCOL_GUID;


EFI_STATUS
EFIAPI
UefiMain (
  IN EFI_HANDLE        ImageHandle,
  IN EFI_SYSTEM_TABLE  *SystemTable
  )
{
  EFI_STATUS Status;

  gIH = ImageHandle;
  gST = SystemTable;
  gBS = SystemTable->BootServices;

  EFI_HANDLE *Handles;
  UINTN NoHandles;
  gBS->LocateHandleBuffer(
    ByProtocol,
    &gEfiDhcp4ServiceBindingProtocolGuid,
    NULL,
    &NoHandles,
    &Handles
    );

  EFI_SERVICE_BINDING_PROTOCOL *Dhcp4Binding;
  EFI_HANDLE Dhcp4Handle;
  EFI_DHCP4_PROTOCOL *Dhcp4 = NULL;
  for (UINTN i = 0; i < NoHandles; i++) {
    Status = gBS->OpenProtocol(
      Handles[i],
      &gEfiDhcp4ServiceBindingProtocolGuid,
      &Dhcp4Binding,
      gIH,
      NULL,
      EFI_OPEN_PROTOCOL_GET_PROTOCOL
      );
    if (Status != EFI_SUCCESS) {
      continue;
    }

    Dhcp4Handle = Handles[i];
    Status = Dhcp4Binding->CreateChild(Dhcp4Binding, &Dhcp4Handle);
    if (Status != EFI_SUCCESS) {
      gBS->CloseProtocol(Handles[i], &gEfiDhcp4ServiceBindingProtocolGuid, gIH, NULL);
      continue;
    }

    Status = gBS->OpenProtocol(
      Dhcp4Handle,
      &gEfiDhcp4ProtocolGuid,
      &Dhcp4,
      gIH,
      NULL,
      EFI_OPEN_PROTOCOL_GET_PROTOCOL
      );
    if (Status != EFI_SUCCESS) {
      Dhcp4Binding->DestroyChild(Dhcp4Binding, Dhcp4Handle);
      gBS->CloseProtocol(Handles[i], &gEfiDhcp4ServiceBindingProtocolGuid, gIH, NULL);
      continue;
    }
    break;
  }
  if (Dhcp4 == NULL) {
    Print(L"can't open dhcp4 protocol\n");
    return EFI_SUCCESS;
  }

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

  Print(L"got ipv4 address\n");
  Print(
    L"%d.%d.%d.%d\n",
    ModeData.ClientAddress.Addr[0],
    ModeData.ClientAddress.Addr[1],
    ModeData.ClientAddress.Addr[2],
    ModeData.ClientAddress.Addr[3]
    );

  Dhcp4->Release(Dhcp4);
  Dhcp4Binding->DestroyChild(Dhcp4Binding, Dhcp4Handle);
  gBS->FreePool(Handles);
  Print(L"Done.\n"); 
  return EFI_SUCCESS;
}
```

Startは非同期にもできる