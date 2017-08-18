---
layout: post
title: 
tags:
- 
---

$ find . -name '*.inf' | xargs grep -h -e 'gEfi.*Guid' > ~/all_guids.txt
$ find /cygdrive/c/projs/edk2/ -name '*.inf' | xargs grep -h -e 'gEfi.*Guid' | grep -v '|' | grep -v '\.' | sed -e 's/#.*//g' | sed -e 's/AND//g' | sed -e 's/ //g' > ~/all_guids.txt

$ find /cygdrive/c/projs/edk2/ -name '*.inf' | xargs grep -h -e 'gEfi.*Guid' | grep -v '|' | grep -v '\.' | sed -e 's/#.*//g' | sed -e 's/AND//g' | sed -e 's/ //g' | tr -d '\r' | sort | uniq > ~/all_guids.txt
genkami@genkami-coconv [~] 2017/08/18 05:27:21
$ wc -l all_guids.txt
600 all_guids.txt


関係あるかは微妙だけど気になる
https://www.sentinelone.com/blog/reverse-engineering-mac-os-x/
