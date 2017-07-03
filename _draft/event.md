event

EFI_EVENT

typedef
EFI_STATUS
CreateEvent (
  IN  UINT32           Type,
  IN  EFI_TPL          NotifyTpl,
  IN  EFI_EVENT_NOTIFY NotifyFunction, OPTIONAL
  IN  VOID             *NotifyContext, OPTIONAL
  OUT EFI_EVENT        *Event
  );

イベントの状態はwaiting or signaledのいずれか
最初はwaitingで、SignalEventされるとsignaledになる

eventのtypeごとにサポートされているtplの種類が違う？
EVT_TIMER SetTimerで使われる
EVT_RUNTIME BootServicesが終了した後でも使われる
EVT_NOTIFY_WAIT waiting時にNotifyFunctionが呼ばれる
EVT_NOTIFY_SIGNAL signal時にNotifyFunctionが呼ばれる
EVT_SIGNAL_EXIT_BOOT_SERVICES ExitBootServices()が呼ばれた時のイベント
EVT_SIGNAL_VIRTUAL_ADDRESS_CHANGE SetVirtualAddressMap()が呼ばれた時のイベント
した二つ以外はorで組み合わせることができる
またEVT_NOTIFY_WAITとEVT_NOTIFY_SIGNALは同時に両方指定することはできない

TPLは割り込みの優先順位
TPL_APPLICATION < TPL_CALLBACK < TPL_NOTIFY < ファームウェア割り込み < TPL_HIGH_LEVEL
の順番で優先度が高い


eventごとにcontextを一つ持ち、そのイベントに関係する関数はcontextをさわることができる
contextの型は任意で、VOID *として渡される

WaitFotEvent(NumEvents, EventArray, &EventIndex)で、そのイベントが来るまで待つ。
EventIndexには、EventArrayのうち何番目のイベントが呼ばれたかが記録される。

#include <Uefi.h>
#include <Library/UefiApplicationEntryPoint.h>
#include <Library/UefiLib.h>

EFI_HANDLE gIH;
EFI_SYSTEM_TABLE *gST;
EFI_BOOT_SERVICES *gBS;

typedef struct EVENT_CONTEXT {
  UINTN Count;
} EVENT_CONTEXT;

void
EFIAPI
EventNotify (
  IN EFI_EVENT Event,
  IN VOID *Context
  )
{
  EVENT_CONTEXT *EventContext = Context;
  EventContext->Count++;
  Print(L"Event notified; Count = %d\n", EventContext->Count);
  return;
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

  EFI_EVENT Event;
  EVENT_CONTEXT EventContext = { .Count = 0 };
  gBS->CreateEvent(
    EVT_NOTIFY_SIGNAL,
    TPL_CALLBACK,
    EventNotify,
    &EventContext,
    &Event
    );

  gBS->SignalEvent(Event);

  Print(L"done.\n");
  gBS->CloseEvent(Event);
  return EFI_SUCCESS;
}




EVT_NOTIFY_SIGNALにしてみたバージョン

void
EFIAPI
EventNotify (
  IN EFI_EVENT Event,
  IN VOID *Context
  )
{
  EVENT_CONTEXT *EventContext = Context;
  EventContext->Count++;
  Print(L"Event is waiting; Count = %d\n", EventContext->Count);
  return;
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

  EFI_EVENT Event;
  EVENT_CONTEXT EventContext = { .Count = 0 };
  gBS->CreateEvent(
    EVT_NOTIFY_WAIT,
    TPL_CALLBACK,
    EventNotify,
    &EventContext,
    &Event
    );

  // Signalせずに待つ
  UINTN EventIndex;
  gBS->WaitForEvent(1, &Event, &EventIndex);

  Print(L"done.\n");
  gBS->CloseEvent(Event);
  return EFI_SUCCESS;
}


また、EFI_BOOT_SERVICES.CheckEvent(Event)で、そのイベントがすでに呼ばれたかどうかを調べることができる。
