#include <Protocol/SimpleNetwork.h>

https://github.com/tianocore/tianocore.github.io/wiki/NetworkPkg-Getting-Started-Guide

opensslをUEFI上にビルドするためには、CryptoPkg/Library/OpensslLib/いかにその辺から引っ張ってきたopensslのソースコードが必要
     > cd CryptoPkg/Library/OpensslLib
     > git clone -b OpenSSL_1_1_0e https://github.com/openssl/openssl openssl
     or
     > git clone https://github.com/openssl/openssl openssl
     > git checkout OpenSSL_1_1_0e



build...
c:\projs\edk2\DuetPkg\DuetPkgIa32.dsc(...): error 4000: Instance of library class [FileExplorerLib] is not found
        in [c:\projs\edk2\NetworkPkg\TlsAuthConfigDxe\TlsAuthConfigDxe.inf] [IA32]
        consumed by module [c:\projs\edk2\NetworkPkg\TlsAuthConfigDxe\TlsAuthConfigDxe.inf]

DuetPkgIa32.dscを編集して、
[Libraries]
FileExplorerLib|/Path/To/FileExplorerLib.inf
を追加

OpensslLib.lib(ssl_asn1.obj) : error LNK2001: 外部シンボル "_ZUINT64_it" は未解決です。
OpensslLib.lib(ssl_asn1.obj) : error LNK2001: 外部シンボル "_ZINT64_it" は未解決です。
OpensslLib.lib(ssl_asn1.obj) : error LNK2001: 外部シンボル "_UINT32_it" は未解決です。
OpensslLib.lib(ssl_asn1.obj) : error LNK2001: 外部シンボル "_ZINT32_it" は未解決です。
OpensslLib.lib(ssl_asn1.obj) : error LNK2001: 外部シンボル "_INT32_it" は未解決です。