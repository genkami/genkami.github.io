---
layout: post
title: CSSで文字のはみ出した部分を「…」で省略する
tags:
- CSS
---

今までは左側のRecent Postsの部分の記事タイトルを文字数指定で切り落としていたのですが、CSSでも同様のことができるみたいです。しかも文字数じゃなくて幅を超えた分だけ省略できるので、見た目も綺麗

``` html
<p id="box">
寿限無寿限無五劫の擦り切れ海砂利水魚の水行末雲来末風来末食う寝る処に住む処藪ら柑子の藪柑子パイポパイポパイポのシューリンガンシューリンガンのグーリンダイグーリンダイのポンポコピーのポンポコナーの長久命の長助
</p>
<style>
#box {
  width: 400px;
  border: 1px solid #000;


  text-overflow: ellipsis; /* 幅を超えた部分を...にする */
  white-space: nowrap;     /* 改行しない */
  overflow: hidden;        /* はみ出した部分は非表示にする */
}
</style>
```

結果↓
<div>
<p id="box">
寿限無寿限無五劫の擦り切れ海砂利水魚の水行末雲来末風来末食う寝る処に住む処藪ら柑子の藪柑子パイポパイポパイポのシューリンガンシューリンガンのグーリンダイグーリンダイのポンポコピーのポンポコナーの長久命の長助
</p>
<style>
#box {
  width: 400px;
  border: 1px solid #000;


  text-overflow: ellipsis;
  white-space: nowrap;
  overflow: hidden;
}
</style>
</div>

詳細は以下を参照

[text-overflow - CSS \| MDN](https://developer.mozilla.org/ja/docs/Web/CSS/text-overflow)