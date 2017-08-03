---
layout: post
title: Debianで最新のnginxを使う
tags:
- Others
---

Debian jessieのnginxはアホみたいに古いので(だったらsid使え)、nginx公式が用意してくれているリポジトリを使って最新版をインストールします。

[Install \| NGINX](https://www.nginx.com/resources/wiki/start/topics/tutorials/install/)

```
deb http://nginx.org/packages/debian/ jessie nginx
deb-src http://nginx.org/packages/debian/ jessie nginx
```

これを`/etc/apt/sources.list`に追加して、

```
$ sudo apt update
...
GPG error: http://nginx.org jessie InRelease: The following signatures couldn't be verified because the public key is not available: NO_PUBKEY XXXXX
```

怒られてしまったので以下を実行。

```
$ sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys XXXXX
$ sudo apt update
```

これで怒られなくなりました。

Debian標準のnginxをこのリポジトリにある新しいもので置き換えます。

```
$ sudo apt remove nginx-full nginx-common
$ suto apt install nginx
```

```
$ sudo service nginx start
Failed to start nginx.service: Unit nginx.service is masked.
```

なぜかmaskされてしまっているようなので、unmaskします。

```
$ sudo systemctl unmask nginx
$ sudo service nginx start
```

これで動いた。
