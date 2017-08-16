---
layout: post
title: VirtualBoxでホスト-ゲスト間のファイル共有
tags:
- Others
---

## ホスト側の設定

「設定」→「共有フォルダ」のメニューから共有フォルダを追加します。

![/img/post/2017-08-17-vbox-share-dir.png](/img/post/2017-08-17-vbox-share-dir.png)

## ゲスト側の設定
### ドライバのインストール
VMを起動し、メニューから「Devices」→「Insert Guest Additions CD Image」を選択します。

するとCDドライブが追加され、(それなりにちゃんとした環境なら)自動再生するかどうか聞かれ、自動再生をすれば勝手にドライバがインストールされます。聞かれなかった場合はがんばってください。

### マウント
```
# mount -t vboxsf (設定した共有フォルダ名) /mnt/share/
```

## 参考

+ [http://qiita.com/skyzhao/items/20e04dff1cf277d330a1](http://qiita.com/skyzhao/items/20e04dff1cf277d330a1)
+ [https://virtualboxes.org/doc/installing-guest-additions-on-ubuntu/](https://virtualboxes.org/doc/installing-guest-additions-on-ubuntu/)
