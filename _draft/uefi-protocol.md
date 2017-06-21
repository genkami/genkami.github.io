protocol 関連のメモ

テキスト入出力

EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL

標準で用意されているのは EFI_SYSTEM_TABLE.ConOut, StdErr
http://wiki.phoenix.com/wiki/index.php/EFI_SIMPLE_TEXT_OUTPUT_PROTOCOL

``` c
#include <Uefi.h>
#include <Library/UefiApplicationEntryPoint.h>
#include <Library/UefiLib.h>

EFI_STATUS
EFIAPI
UefiMain (
  IN EFI_HANDLE        ImageHandle,
  IN EFI_SYSTEM_TABLE  *SystemTable
  )
{
	SystemTable->ConOut->OutputString(SystemTable->ConOut, L"Hello, world\n");
	return EFI_SUCCESS;
}
```

Printマクロ

``` c
#include <Uefi.h>
#include <Library/UefiApplicationEntryPoint.h>
#include <Library/UefiLib.h>

EFI_STATUS
EFIAPI
UefiMain (
  IN EFI_HANDLE        ImageHandle,
  IN EFI_SYSTEM_TABLE  *SystemTable
  )
{
	Print(L"Hello, %s!\n", L"world"); 
	return EFI_SUCCESS;
}
```
EDK2のみ？

```
/** @file
  Provides services to print a formatted string to a buffer. All combinations of
  Unicode and ASCII strings are supported.

Copyright (c) 2006 - 2017, Intel Corporation. All rights reserved.<BR>
This program and the accompanying materials are licensed and made available under 
the terms and conditions of the BSD License that accompanies this distribution.  
The full text of the license may be found at
http://opensource.org/licenses/bsd-license.php.

THE PROGRAM IS DISTRIBUTED UNDER THE BSD LICENSE ON AN "AS IS" BASIS,
WITHOUT WARRANTIES OR REPRESENTATIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED.

  The Print Library functions provide a simple means to produce formatted output 
  strings.  Many of the output functions use a format string to describe how to 
  format the output of variable arguments.  The format string consists of normal 
  text and argument descriptors.  There are no restrictions for how the normal 
  text and argument descriptors can be mixed.  The following end of line(EOL) 
  translations must be performed on the contents of the format string:
  
     - '\\r' is translated to '\\r'
     - '\\r\\n' is translated to '\\r\\n'
     - '\\n' is translated to '\\r\\n' 
     - '\\n\\r' is translated to '\\r\\n'
  
  This does not follow the ANSI C standard for sprint().  The format of argument 
  descriptors is described below.  The ANSI C standard for sprint() has been 
  followed for some of the format types, and has not been followed for others.  
  The exceptions are noted below.

    %[flags][width][.precision]type

  [flags]:
    - -       
      - The field is left justified.  If not flag is not specified, then the 
        field is right justified.
    - space   
      - Prefix a space character to a number.  Only valid for types X, x, and d.
    - + 
      - Prefix a plus character to a number.  Only valid for types X, x, and d.  
        If both space and + are specified, then space is ignored.
    - 0
      - Pad with 0 characters to the left of a number.  Only valid for types 
        X, x, and d.
    - ,
      - Place a comma every 3rd digit of the number.  Only valid for type d.
        If 0 is also specified, then 0 is ignored.
    - L, l
      - The number being printed is size UINT64.  Only valid for types X, x, and d.
        If this flag is not specified, then the number being printed is size int.
    - NOTE: All invalid flags are ignored.

  [width]:

    - *
      - The width of the field is specified by a UINTN argument in the 
        argument list.
    - number
      - The number specified as a decimal value represents the width of 
        the field.
    - NOTE: If [width] is not specified, then a field width of 0 is assumed.

  [.precision]:

    - *
      - The precision of the field is specified by a UINTN argument in the 
        argument list.
    - number
      - The number specified as a decimal value represents the precision of 
        the field.
    - NOTE: If [.precision] is not specified, then a precision of 0 is assumed.

  type:

    - %
      - Print a %%.
    - c
      - The argument is a Unicode character.  ASCII characters can be printed 
        using this type too by making sure bits 8..15 of the argument are set to 0.
    - x
      - The argument is an unsigned hexadecimal number.  The characters used are 0..9 and 
        A..F.  If the flag 'L' is not specified, then the argument is assumed 
        to be size int.  This does not follow ANSI C.
    - X
      - The argument is an unsigned hexadecimal number and the number is padded with 
        zeros.  This is equivalent to a format string of "0x". If the flag 
        'L' is not specified, then the argument is assumed to be size int.  
        This does not follow ANSI C.
    - d
      - The argument is a signed decimal number.  If the flag 'L' is not specified, 
        then the argument is assumed to be size int.  
    - u
      - The argument is a unsigned decimal number.  If the flag 'L' is not specified, 
        then the argument is assumed to be size int.
    - p
      - The argument is a pointer that is a (VOID *), and it is printed as an 
        unsigned hexadecimal number  The characters used are 0..9 and A..F.
    - a
      - The argument is a pointer to an ASCII string.  
        This does not follow ANSI C.
    - S, s
      - The argument is a pointer to a Unicode string.  
        This does not follow ANSI C.
    - g
      - The argument is a pointer to a GUID structure.  The GUID is printed 
        in the format XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX.  
        This does not follow ANSI C.
    - t
      - The argument is a pointer to an EFI_TIME structure.  The time and 
        date are printed in the format "mm/dd/yyyy hh:mm" where mm is the 
        month zero padded, dd is the day zero padded, yyyy is the year zero 
        padded, hh is the hour zero padded, and mm is minutes zero padded.  
        This does not follow ANSI C. 
    - r
      - The argument is a RETURN_STATUS value.  This value is converted to 
        a string following the table below.  This does not follow ANSI C. 
      - RETURN_SUCCESS               
        - "Success"
      - RETURN_LOAD_ERROR            
        - "Load Error"
      - RETURN_INVALID_PARAMETER     
        - "Invalid Parameter"
      - RETURN_UNSUPPORTED           
        - "Unsupported"
      - RETURN_BAD_BUFFER_SIZE       
        - "Bad Buffer Size"
      - RETURN_BUFFER_TOO_SMALL      
        - "Buffer Too Small"
      - RETURN_NOT_READY             
        - "Not Ready"
      - RETURN_DEVICE_ERROR          
        - "Device Error"
      - RETURN_WRITE_PROTECTED       
        - "Write Protected"
      - RETURN_OUT_OF_RESOURCES      
        - "Out of Resources"
      - RETURN_VOLUME_CORRUPTED      
        - "Volume Corrupt"
      - RETURN_VOLUME_FULL           
        - "Volume Full"
      - RETURN_NO_MEDIA              
        - "No Media"
      - RETURN_MEDIA_CHANGED         
        - "Media changed"
      - RETURN_NOT_FOUND             
        - "Not Found"
      - RETURN_ACCESS_DENIED         
        - "Access Denied"
      - RETURN_NO_RESPONSE           
        - "No Response"
      - RETURN_NO_MAPPING            
        - "No mapping"
      - RETURN_TIMEOUT               
        - "Time out"
      - RETURN_NOT_STARTED           
        - "Not started"
      - RETURN_ALREADY_STARTED       
        - "Already started"
      - RETURN_ABORTED               
        - "Aborted"
      - RETURN_ICMP_ERROR            
        - "ICMP Error"
      - RETURN_TFTP_ERROR            
        - "TFTP Error"
      - RETURN_PROTOCOL_ERROR        
        - "Protocol Error"
      - RETURN_WARN_UNKNOWN_GLYPH    
        - "Warning Unknown Glyph"
      - RETURN_WARN_DELETE_FAILURE   
        - "Warning Delete Failure"
      - RETURN_WARN_WRITE_FAILURE    
        - "Warning Write Failure"
      - RETURN_WARN_BUFFER_TOO_SMALL 
        - "Warning Buffer Too Small"

**/
```
Library/UefiLib.h で Print が定義されている
MdePkg/Include/Library/PrintLib.h からの引用
sprintf的なやつとかはLibrary/PrintLib.h の UnicodeSPrintとか

入力
EFI_SIMPLE_TEXT_INPUT_PROTOCOL

ReadKeyStrokeという1文字読む関数しかない
```
#include <Uefi.h>
#include <Library/UefiApplicationEntryPoint.h>
#include <Library/UefiLib.h>

EFI_SYSTEM_TABLE *gST;
EFI_BOOT_SERVICES *gBS;

EFI_STATUS GetLine (
	OUT CHAR16 *Buf,
	IN OUT UINTN *BufSize
	)
{
	EFI_STATUS Status;
	UINTN EventIndex;
	EFI_INPUT_KEY Key;
	for (UINTN i = 0; i < *BufSize; i++) {
		gBS->WaitForEvent(1, &gST->ConIn->WaitForKey, &EventIndex);
		Status = gST->ConIn->ReadKeyStroke(gST->ConIn, &Key);
		if (Status != EFI_SUCCESS) {
			return Status;
		}
		Buf[i] = Key.UnicodeChar;
		if (Buf[i] == L'\n' || Buf[i] == L'\r') {
			Print(L"\n");
			Buf[i] = L'\0';
			*BufSize = i;
			return EFI_SUCCESS;
		}
		Print(L"%c", Buf[i]);

	}
	return EFI_BUFFER_TOO_SMALL;
}

#define BUF_SIZE 256

EFI_STATUS
EFIAPI
UefiMain (
  IN EFI_HANDLE        ImageHandle,
  IN EFI_SYSTEM_TABLE  *SystemTable
  )
{
	CHAR16 *Buf;
	UINTN BufSize = BUF_SIZE;
	EFI_STATUS Status;

	gST = SystemTable;
	gBS = SystemTable->BootServices;

	Status = gBS->AllocatePool(EfiLoaderData, BUF_SIZE, &Buf);
	if (Status != EFI_SUCCESS) {
		Print(L"Memory allocation failed\n");
		return Status;
	}

	Print(L"input here: ");

	Status = GetLine(Buf, &BufSize);
	if (Status != EFI_SUCCESS) {
		Print(L"error: %d\n", Status);
		return Status;
	}

	Print(L"you wrote: %s\n", Buf);

	return EFI_SUCCESS;
}
```

```
input here: hoge
you wrote: hoge
```