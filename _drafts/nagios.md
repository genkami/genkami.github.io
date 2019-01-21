---
layout: post
title: Nagios入門
tags:
- Linux
---

## Nagiosとは

スケーラビリティと柔軟性を意識した監視ツール
いろいろなメトリクスを収集して通知することができる

https://www.nagios.org/about/overview/

## インストール

[Quickstart Installation Guides · Nagios Core Documentation](https://assets.nagios.com/downloads/nagioscore/docs/nagioscore/4/en/quickstart.html)

これを見て環境ごとにいい感じに。今回はUbuntuなので以下のような感じで。

```sh
$ docker run -it ubuntu:bionic
root@f85bef3a99e1:/# apt-get update
root@f85bef3a99e1:/# apt-get install -y autoconf gcc libc6 make wget unzip apache2 php libapache2-mod-php7.2 libgd-dev
```

