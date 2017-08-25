---
layout: post
title: 
tags:
- 
---
EFI_HANDLE Handle = NULL;
InstallProtocolInterface(&Handle, &ProtocolGuid, EFI_NATIVE_INTERFACE, Interface);

or
EFI_HANDLE Handle = NULL;
EFI_SERVICE_BINDING_PROTOCOL *HogeSB;
HogeSB->CreateChild(HogeSB, &Handle);
