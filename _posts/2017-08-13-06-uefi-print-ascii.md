---
layout: post
title: UEFIでCHAR8の文字列を表示する
tags:
- UEFI
- C
---

てっきり文字列の出力は`CHAR16`でしかできないものだと思っていたのですが、EDK2では`UefiLib.h`に ある`AsciiPrint`を使うことで、`CHAR8`のascii文字列を表示することができます。

```c
UINTN
EFIAPI
AsciiPrint (
  IN CONST CHAR8  *Format,
  ...
  );
```

使い方は`Print`と同様。
