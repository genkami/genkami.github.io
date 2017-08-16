---
layout: post
title: Trampが起動時に失敗してコケるのを防ぐ
tags:
- Emacs
---

ある日、trampを使ってサーバー上のファイルを書き換えた後にemacsを終了させ、再起動しようとすると、以下のようなエラーが出てしまうようになりました。

```
Tramp failed to connect.  If this happens repeatedly, try
    ‘M-x tramp-cleanup-this-connection’
Tramp failed to connect.  If this happens repeatedly, try
    ‘M-x tramp-cleanup-this-connection’
```

どうやらこれはtramp自体の問題というよりは、recentfが履歴にあるファイルが存在するかチェック→trampで開いたファイルがあるかどうかsshで確認→ポート番号やらが間違ってたみたいで死ぬという謎の現象が起きているようです。

以下を`.emacs.el`に追加して、recentfのcleanupを行わないようにすることで、対症療法的に問題を解決することができます。

```elisp
(setq recentf-auto-cleanup 'never)
```

個人的には、recentfのcleanupは特に必要だと感じていないので、これで大丈夫でした。むしろ起動速度も上がって一石二鳥。


参考: [https://stackoverflow.com/questions/880625/stop-tramp-mode-running-on-emacs-startup](https://stackoverflow.com/questions/880625/stop-tramp-mode-running-on-emacs-startup)
