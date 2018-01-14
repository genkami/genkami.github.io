---
layout: post
title: Jekyllで下書き管理
tags:
- Jekyll
---

あまり知られていないようですが，実はJekyllには下書き管理機能があります．

サイトのルートに`_drafts`ディレクトリを作っておくと，その中に入っている`.md`等のファイルをJekyllは下書きと認識します．

これらの下書きファイルは，`_config.yml`に

```yaml
show_drafts: true
```

を設定するか，`--drafts`オプション付きでJekyllを起動することで通常の記事と同様に表示されるようになります．

また，下書きファイルは通常の`_posts`のファイルとは異なり，ファイル名の先頭に日付を入れる必要はありません．この場合，その記事の日付はファイルの最終更新時刻が適用されます．

参考: [Working with drafts \| jekyll](https://jekyllrb.com/docs/drafts/)
