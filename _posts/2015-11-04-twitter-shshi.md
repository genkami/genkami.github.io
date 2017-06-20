---
layout: post
title: Twitterのふぁぼマークを寿司にする
tags:
- Twitter
---
Twitterのふぁぼマークがハートになってしまったので、昔ながらの寿司アイコンに戻す方法を書いておきます。(Google Chromeのみ)

# 1. Stylebotをインストール
[Stylebot](https://chrome.google.com/webstore/detail/stylebot/oiaejidbmkiecgbjeifoejpgmdaleoha)というChromeの拡張機能をインストールします。

[https://chrome.google.com/webstore/detail/stylebot/oiaejidbmkiecgbjeifoejpgmdaleoha](https://chrome.google.com/webstore/detail/stylebot/oiaejidbmkiecgbjeifoejpgmdaleoha)

上のリンクをクリックし、「CHROMEに追加」ボタンを押します。
![スクリーンショット 2015-11-04 3.18.28.png](/img/post/2015-11-04-https-qiita-image-store.s3.amazonaws.com-0-55949-8d47f061-2268-6dda-e5ee-4e19e234faf6.png)

「Stylebot」を追加しますか? と聞かれるので、「拡張機能を追加」をクリックします。

![スクリーンショット 2015-11-04 3.18.38.png](/img/post/2015-11-04-https-qiita-image-store.s3.amazonaws.com-0-55949-08d9241a-77c1-06ab-b46f-62c185cde55f.png)

インストール自体は数秒で完了すると思います。インストールが完了したらTwitterのページを開いてください。

# 2. TwitterのCSSを書き換える
Twitterのページを開きます。画面右上に「CSS」と書かれたボタンが増えていることに気付くと思います。
![スクリーンショット 2015-11-04 3.19.23.png](/img/post/2015-11-04-https-qiita-image-store.s3.amazonaws.com-0-55949-0bc344ca-9ff6-37c9-4275-c8ce3b3377c7.png)

このボタンをクリックするとメニューが出てくると思うので、「Open Stylebot...」をクリックします。

![スクリーンショット 2015-11-04 3.19.39.png](/img/post/2015-11-04-https-qiita-image-store.s3.amazonaws.com-0-55949-eaffea9e-057e-bc09-6d31-34730e62afd1.png)

クリックすると画面右側によくわからないメニューが出てきますが、無視して下のほうにある「Edit CSS」をクリックします。

![スクリーンショット 2015-11-04 3.19.43.png](/img/post/2015-11-04-https-qiita-image-store.s3.amazonaws.com-0-55949-8da90b53-dfae-a196-87d1-1d74acac6349.png)

すると何か入力できそうな真っ白な画面になるので、以下のCSSをそのままコピペします。

``` css
.HeartAnimation {
    background: url(https://pbs.twimg.com/media/CS5_8eqU8AAkYM7.png:large);
    background-size: 2900%;
    background-position: left;
}

.ProfileTweet-action--favorite .ProfileTweet-actionButton:hover .HeartAnimation, .ProfileTweet-action--favorite .ProfileTweet-actionButton:focus .HeartAnimation, .favorited .ProfileTweet-action--favorite .HeartAnimation {
    background-position: right;
}
```

コピペした結果がこちら

![スクリーンショット 2015-11-04 3.42.43.png](/img/post/2015-11-04-https-qiita-image-store.s3.amazonaws.com-0-55949-70d619c9-57a1-d71e-fcff-c8bf32417d7f.png)

下のほうにある「Save」ボタンを押すとあら不思議! ふぁぼマークがハートから昔ながらの寿司アイコンに戻っています。やったね!

![スクリーンショット 2015-11-04 3.43.14.png](/img/post/2015-11-04-https-qiita-image-store.s3.amazonaws.com-0-55949-2b5b6219-7ffc-4ebc-8b76-432cdb49b3d6.png)

もし動かなくなってたらTwitter社の陰謀か仕様変更だと思ってください。

ちなみにChrome以外のブラウザにもユーザースタイルシートを設定できる拡張機能はあるはずなので、適当に探して適当にやってみたら寿司にできます。
