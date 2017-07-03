qemu-keyboard

```
> qemu-system-x86_64 -hda c:\path\to\arch.img -cdrom c:\path\to\archlinux-install.iso -boot d -m 512
```

途中からキーボードが効かなくなる。
全然関係ないキーが押されてる（アルファベットのキーを押したのにenterとか)

-k japanese でレイアウト指定したら正常に動くようになった