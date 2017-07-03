TypeがEVT_TIMERであるEFI_EVENTを使ってタイマー処理が行える。

typedef
EFI_STATUS
SetTimer (
  IN EFI_EVENT       Event,
  IN EFI_TIMER_DELAY Type,
  IN UINT64          TriggerTime
  );

Event: TypeがEVT_TIMERを含むイベント
Type: タイマーの種類。
  TimerCancel: Eventに設定されているタイマーをキャンセル
  TimerPeriodic: 一定時間ごとにイベントをsignal
  TimerRelative: 一定時間後に一度だけsignal
TriggerTime: 100ns単位の時間。イベントがsignalされる間隔を表す。

``` c
#include <Uefi.h>
#include <Library/UefiApplicationEntryPoint.h>
#include <Library/UefiLib.h>

#define USEC 10
#define MSEC (1000 * USEC)
#define SEC (1000 * MSEC)

EFI_HANDLE gIH;
EFI_SYSTEM_TABLE *gST;
EFI_BOOT_SERVICES *gBS;

typedef struct EVENT_CONTEXT {
  UINTN Count;
} EVENT_CONTEXT;

EFI_EVENT TimerStopped;

void
EFIAPI
EventNotify (
  IN EFI_EVENT Event,
  IN VOID *Context
  )
{
  EVENT_CONTEXT *EventContext = Context;
  EventContext->Count++;
  Print(L"Event signaled; Count = %d\n", EventContext->Count);
  if (EventContext->Count >= 10) {
    gBS->SetTimer(Event, TimerCancel, 0);
    gBS->SignalEvent(TimerStopped);
  }
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

  EFI_STATUS Status;

  Status = gBS->CreateEvent(0, TPL_CALLBACK, NULL, NULL, &TimerStopped);

  EFI_EVENT TimerEvent;
  EVENT_CONTEXT EventContext = { .Count = 0 };
  Status = gBS->CreateEvent(
    EVT_TIMER | EVT_NOTIFY_SIGNAL,
    TPL_CALLBACK,
    EventNotify,
    &EventContext,
    &TimerEvent
    );
  
  Status = gBS->SetTimer(TimerEvent, TimerPeriodic, 500 * MSEC);

  UINTN EventIndex;
  gBS->WaitForEvent(1, &TimerStopped, &EventIndex);

  Print(L"Timer Stopped.\n");

  gBS->CloseEvent(TimerEvent);
  gBS->CloseEvent(TimerStopped);
  return EFI_SUCCESS;
}
```


500msecごとにTimerEventをsignalして、10回signalされたらTimerEventの繰り返しを止めて、TimerStoppedをsignal
mainのほうがTimerStoppedが来るまで待ち、その後終了。