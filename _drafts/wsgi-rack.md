---
layout: post
title: WSGIとRack
tags:
- Ruby
- Python
---

## 基本的な考え方

Webアプリケーションフレームワークを用いた開発

いわゆるMVC

M: ビジネスロジック
V: 表示部分
C: ユーザーのリクエストの処理

Webアプリケーションを作る上で重要な部分であるにもかかわらず、この3つの中に属していないもの

実際にHTTPプロトコルを喋る部分

便宜上これをサーバー部分と呼ぶ。

このサーバー部分と、Webアプリケーションフレームワークを用いて書かれたMVCの部分(アプリケーション部分と呼ぶ)との間のやり取りの方法を共通化する動きが各言語にある

その共通化の方式が、PythonだとWSGI、RubyだとRack

サーバー部分とアプリケーション部分のやり取りの方法を共通化しておくことによって、サーバー部分の都合でWebアプリケーションフレームワークを選んだり、逆にWebアプリケーションフレームワーク側の都合でサーバーを選んだりする必要がなくなる。

WSGIもRackも、その仕様自体はかなり単純。詳細は後述

## WSGI
http://wsgi.readthedocs.io/en/latest/

WSGIのプロトコルは、リクエストの情報である `environ` と、関数 `start_response` の2つの引数を受け取り、レスポンスボディのデータ片を順に返すイテレータを返す関数(もしくはcallableなオブジェクト)として定義される。以下は簡単なWSGIアプリケーションの例:


``` python
def my_wsgi_application(enviorn, start_response):
    start_response(200, [('Content-Type', 'text/html')])
    return [
        '<html lang="ja">',
        '<head><meta charset="utf-8"></head>',
        '<body><h1>It Works!</h1></body>',
        '</html>'
    ]
```

WSGIのサーバー部分のデフォルトの実装としては、Pythonの組み込みモジュールであるwsgirefがある。その他のサーバー部分の実装としては、

その他は、[uWSGI](https://uwsgi-docs.readthedocs.io/en/latest/)や[Gunicorn](http://gunicorn.org/)、Apacheの[mod_wsgi](https://modwsgi.readthedocs.io/en/develop/)などがある。

WSGIに準拠したWebアプリケーションフレームワークとしては、[Flask](http://flask.pocoo.org/)や[Django](https://www.djangoproject.com/)、[Bottle](https://bottlepy.org/docs/dev/)など。

## Rack

https://github.com/rack/rack

RackのプロトコルもWSGIに似ている。Rackのプロトコルは、関数 `env` を受け取り、ステータスとレスポンスヘッダ、ボディかもしくはボディののデータ片を順に返すイテレータの3つの返り値を返すメソッド `call` を持つクラスとして定義される。


``` ruby
class MyRackApp
  def call(env)
    [200,
     {'Content-Type' => 'text/html'},
     ['<html lang="ja">',
      '<head><meta charset="utf-8"></head>',
      '<body><h1>It Works!</h1></body>',
      '</html']
  end
end
```

Rackのサーバー部分としては、デフォルトで用意されているものはWEBrick。その他には[Thin](https://github.com/macournoyer/thin/)や[Unicorn](https://bogomips.org/unicorn/)などがある。

Rackに準拠したWebアプリケーションフレームワークとしては、言わずと知れた[Rails](http://rubyonrails.org/)や[Padrino](http://padrinorb.com/)などがある。

RackはWSGIと違って、コマンドラインツール群も提供されている。

## Web他
ElixirならPlug、HaskellならWAI等、言語によらずWSGIやRackに似た考え方を用いることが主流になっている。
