---
layout: post
title: Macで$HOSTNAMEとhostnameを一致させる
tags:
- Mac
---
Macでのホスト名の設定は2種類あるようです。

1つめが`$HOSTNAME`に保存されている値。こちらは、システム設定→共有から変更できます。

もう一つが`hostname`コマンドの表示する値。Emacsの`(system-name)`もこちらを表示します。こちらの方のホスト名は以下のコマンドで変更することができます。


``` shell
$ sudo scutil -–set HostName NEW_HOST_NAME
```

おわり。