---
layout: post
title: Twitterのアイコンを寿司にする
tags:
- Twitter
---

Twitterのアイコンが丸くなってしまったので、昔ながらの寿司に戻す方法を紹介します。

## Firefoxの場合
### 1. Greasemonkeyをインストールする
まずは[Greasemonkey](https://addons.mozilla.org/ja/firefox/addon/greasemonkey/)をインストールします。

![2017-06-19-sushi-01.png](/img/post/2017-06-19-sushi-01.png)

インストールボタンを押して再起動すれば準備完了です。

### 2. スクリプトを追加する

![2017-06-19-sushi-02.png](/img/post/2017-06-19-sushi-02.png)

再起動が終わればFirefoxの右上にキモい猿みたいなアイコンが出てくると思うので、その横にある三角形のボタンを押し、「ユーザースクリプト新規作成」をクリックします。

すると次のようなウィンドウが出てくると思います。

![2017-06-19-sushi-03.png](/img/post/2017-06-19-sushi-03.png)

「名前」「名前空間」「説明」のところはなんでもいいので、sushiとでも入れておきましょう。重要なのは、「実行するページ」の欄に「https://twitter.com/」と入力することです。

入力が終わったらOKを押しましょう。次のようなウィンドウが出てくるはずです。

![2017-06-19-sushi-04.png](/img/post/2017-06-19-sushi-04.png)

この色々書いてるやつの一番下に、以下のコードをコピペします。

``` javascript
var ob = new MutationObserver(function (rec, self) {
  $('.avatar, .Avatar, .js-action-profile-avatar').attr('src', 'https://abs.twimg.com/emoji/v2/72x72/1f363.png');
});
ob.observe(document.body, { subtree: true, childList: true });
```

コピペが終わったら「保存」を押して、ウィンドウを閉じましょう。そして[Twitterのページ](https://twitter.com/)を開くと……

![2017-06-19-sushi-05.png](/img/post/2017-06-19-sushi-05.png)

このようにアイコンが昔ながらの寿司に戻っているはずです。

## Firefox以外の場合
Firefox以外のブラウザでも、Greasemonkey相当のことができるアドオンがあるようです。

+ Chrome: [Tampermonkey](https://chrome.google.com/webstore/detail/tampermonkey/dhdgffkkebhmkfjojejmpbldmpobfkfo?hl=ja)
+ Safari: [Tampermonkey](https://tampermonkey.net/?browser=safari)

参考: [http://qiita.com/GODVA_GOBBA/items/34fd127578ddb91dfd82](http://qiita.com/GODVA_GOBBA/items/34fd127578ddb91dfd82)