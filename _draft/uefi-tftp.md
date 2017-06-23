#include <Protocol/SimpleNetwork.h>

https://github.com/tianocore/tianocore.github.io/wiki/NetworkPkg-Getting-Started-Guide

UEFIのSpecによると、EFI_NETWORK_INTERFACE_IDENTIFIER_PROTOCOL -> EFI_SIMPLE_NETWORK_PROTOCOL or EFI_MANAGED_NETWORK_PROTOCOL -> EFI_PXE_BASE_PROTOCOLの順にろーどしていけるはず

EFI_SERVICE_BINDING_PROTOCOL.CreateChild()を使って呼び出すプロトコルもある

VirtualBoxでPXEを有効化しないといけなそう
できてた
PCnet-Fast III なら認識されてくれそうな感じだけど、されてくれない
そもそもなんか違うことやってるかも

http://linux2.g.hatena.ne.jp/lnznt/20120203/1328259914

とりあえずiPXEが今やっていることに近そうなんでiPXEのソースコード読んでみる

iPXEがmakeできない


/tmp/ccjtNZ1n.s: Assembler messages:
/tmp/ccjtNZ1n.s:21: Error: junk at end of line, first unrecognized character is `"'
/tmp/ccjtNZ1n.s:37: Error: unknown pseudo-op: `.previous'
/tmp/ccjtNZ1n.s:85: Error: junk at end of line, first unrecognized character is `"'
/tmp/ccjtNZ1n.s:101: Error: unknown pseudo-op: `.previous'
/tmp/ccjtNZ1n.s:130: Error: junk at end of line, first unrecognized character is `"'
/tmp/ccjtNZ1n.s:146: Error: unknown pseudo-op: `.previous'
/tmp/ccjtNZ1n.s:194: Error: can't resolve `' { section} - `LFB107' {.text$null_san_hook section}
/tmp/ccjtNZ1n.s:210: Error: can't resolve `' { section} - `LFB109' {.text$null_san_boot section}
/tmp/ccjtNZ1n.s:218: Error: can't resolve `' { section} - `LFB110' {.text$null_san_describe section}
/tmp/ccjtNZ1n.s:544: Error: can't resolve `' { section} - `LFB107' {.text$null_san_hook section}
/tmp/ccjtNZ1n.s:603: Error: can't resolve `' { section} - `LFB109' {.text$null_san_boot section}
/tmp/ccjtNZ1n.s:627: Error: can't resolve `' { section} - `LFB110' {.text$null_san_describe section}
/tmp/ccjtNZ1n.s:1065: Error: can't resolve `' { section} - `LFB107' {.text$null_san_hook section}
/tmp/ccjtNZ1n.s:1069: Error: can't resolve `' { section} - `LFB109' {.text$null_san_boot section}
/tmp/ccjtNZ1n.s:1071: Error: can't resolve `' { section} - `LFB110' {.text$null_san_describe section}
make: *** [Makefile.housekeeping:916: bin/null_sanboot.o] エラー 1


http://forum.ipxe.org/showthread.php?tid=10007

http://ipxe.org/download
$ cd src/
$ make bin-x86-efi/ipxe.efi