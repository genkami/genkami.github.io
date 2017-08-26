---
layout: post
title: UEFIでEFI_HANDLEを作る
tags:
- UEFI
- C
---

ドライバを書いている時などは新しい`EFI_HANDLE`を作りたくなる場面がよくありますが、これを実現する方法は3つあります。

### 1. InstallProtocolInterfaceにNULLを渡す

`EFI_BOOT_SERVICES.InstallProtocolInterface`に`NULL`は`EFI_HANDLE`を渡すと、新しい`EFI_HANDLE`を作って代入してくれます。

```
EFI_HANDLE Handle = NULL;
gBS->InstallProtocolInterface(&Handle, &ProtocolGuid, EFI_NATIVE_INTERFACE, Interface);
```

### 2. InstallMultipleProtocolInterfacesにNULLを渡す

これも同様。

```
EFI_HANDLE Handle = NULL;
gBS->InstallMultipleProtocolInterfaces(&Handle, &ProtocolGuid, Interface, ...);
```

### 3. EFI_SERVICE_BINDING_PROTOCOL.CreateChildにNULLを渡す

これも同様。

```
EFI_HANDLE Handle = NULL;
EFI_SERVICE_BINDING_PROTOCOL *HogeSB = ...;
HogeSB->CreateChild(HogeSB, &Handle);
```
