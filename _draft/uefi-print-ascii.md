---
layout: post
title: 
tags:
- 
---

uefiでascii文字列を表示

UefiLib.h に AsciiPrint がある

UINTN
EFIAPI
AsciiPrint (
  IN CONST CHAR8  *Format,
  ...
  );

