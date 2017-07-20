---
layout: post
title: Boot Servicesが終わった後
tags:
- UEFI
---

`EFI_BOOT_SERVICES.ExitBootServices()`が呼ばれると、Boot Servicesが終了します。

```c
typedef
EFI_STATUS
(EFIAPI *EFI_EXIT_BOOT_SERVICES) (
     IN EFI_HANDLE ImageHandle,
     IN UINTN MapKey
);
```

引数は以下のような意味になっています。

+ `ImageHandle`: 実行中のアプリケーションの`ImageHandle`
+ `MapKey`: `EFI_BOOT_SERVICES.GetMemoryMap()`で取得した`MapKey`

メモリの状態の整合性が取れなくなる可能性があるので、`ExitBootServices()`をする場合はその直前に`GetMemoryMap()`を呼び、そこで得た`MapKey`を渡さなければなりません。

`ExitBootServices()`は以下のような処理を行います。

+ `EVT_SIGNAL_EXIT_BOOT_SERVICES`が指定されているイベントを(まだ発火されていなければ)発火する
+ `EFI_SYSTEM_TABLE`のうち、以後使用できなくなる部分(`Con*`, `Console*`, `BootServices`)を消す
+ Memory Typeが`EfiBootServicesData`, `EfiBootServicesCode`である領域の開放

このような処理が行われるため、一度`ExitBootServices()`を読んだ後は、仮に`ExitBootServices()`が失敗したとしても、その後`GetMemoryMap()`以外のBoot Servicesの関数を呼び出してはいけません。

また、Boot Services Driversの提供する機能は、以後使えなくなります。

