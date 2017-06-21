efiapi
Unless otherwise stated, all functions defined in the UEFI specification are called through pointers
in common, architecturally defined, calling conventions found in C compilers. Pointers to the
various global UEFI functions are found in the EFI_RUNTIME_SERVICES and
EFI_BOOT_SERVICES tables that are located via the system table. Pointers to other functions
defined in this specification are located dynamically through device handles. In all cases, all pointers
to UEFI functions are cast with the word EFIAPI. This allows the compiler for each architecture to
supply the proper compiler keywords to achieve the needed calling conventions.

IN 関数が引数として受け取る値であることを明示
OUT 関数が戻り値としてこの引数がさす値を書き換えることを明示
OPTIONAL NULLでもいいよ
CONST 読み込み専用
