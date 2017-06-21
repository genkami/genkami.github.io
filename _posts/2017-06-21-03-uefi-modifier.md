---
layout: post
title: UEFI関連のソースコードについてる謎修飾子の解説
tags:
- UEFI
- C
---
UEFI用のドライバやらのソースコードを読んでいると、`EFIAPI`とか`IN`, `OUT`とかの謎修飾子がコードについていたりします。

## EFIAPI
UEFIでのコードは、基本的に`EFI_BOOT_SERVICES`や`EFI_RUNTIME_SERVICE`から動的に関数やデータ構造を読み込んで使用するという流れになっています。`EFIAPI`は指定した関数がこういった使われ方をする可能性があるということを明示的に指定する修飾子です。

``` c
EFI_STATUS EFIAPI UefiMain (...) { ... }
```

## IN, OUT
`IN`は関数が引数として受け取る値であることを明示する修飾子で、`OUT`は渡したポインタの指す先が書き換えられる可能性があることを示す修飾子です。

``` c
typedef EFI_STATUS (EFIAPI *EFI_WAIT_FOR_EVENT) (
    IN UINTN NumberOfEvents,
    IN EFI_EVENT *Event,
    OUT UINTN *Index
    );
```

## OPTIONAL
`OPTIONAL`は、場合によっては引数が`NULL`でも構わないことを明示する修飾子です。

``` c
typedef EFI_STATUS (EFIAPI *EFI_LOCATE_HANDLE) (
    IN EFI_LOCATE_SEARCH_TYPE SearchType,
    IN EFI_GUID *Protocol OPTIONAL,
    IN VOID *SearchKey OPTIONAL,
    IN OUT UINTN *BufferSize,
    OUT EFI_HANDLE *Buffer
 );
```

## CONST
`CONST`は、引数で与えられた値が書き変わることはないということを明示する修飾子です。

``` c
typedef UINTN (EFIAPI *EFI_DEVICE_PATH_UTILS_GET_DEVICE_PATH_SIZE) (
    IN CONST EFI_DEVICE_PATH_PROTOCOL *DevicePath
    );
```