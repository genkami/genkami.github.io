---
layout: post
title: 
tags:
- 
---

sudo VBoxManage internalcommands createrawvmdk -filename hoge.vmdk -rawdisk /dev/hoge

VERR_RESOURCE_BUSY

アンマウントしてるのにエラー

sudo lsof /dev/disk2s1

spotlightが悪さしてた

sudo diskutil unmount /dev/disk2s1

