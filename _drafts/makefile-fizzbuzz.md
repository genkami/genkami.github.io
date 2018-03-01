---
layout: post
title: GNU Make"のみ"でFizz Buzz
tags:
- Linux
---

1. https://gist.github.com/g-k/4381991
2. http://d.hatena.ne.jp/eel3/20110924/1316791928
3. https://rosettacode.org/wiki/FizzBuzz#make

1はgmslとかいうチートを使っている。整数の比較とかができるので楽勝

2は`seq`コマンドを使って3の倍数の列や5の倍数の列などを生成している。`filter-out`以外は実質シェルスクリプト。


3は`jot`や`expr`などのコマンドを多用。実質シェルスクリプト。


```make
MAX := xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

all: fb-$(MAX)x

define ITER_NUMS
$(if $1,$(call ITER_NUMS,$(patsubst %x,%,$1)) $1,)
endef

ALL_NUMS := $(call ITER_NUMS,$(MAX))

define FIZZ
$(if $(findstring xxx,$1),\
	$(call FIZZ,$(patsubst %xxx,%,$1)),\
	$(if $1,,Fizz))
endef

define BUZZ
$(if $(findstring xxxxx,$1),\
	$(call BUZZ,$(patsubst %xxxxx,%,$1)),\
	$(if $1,,Buzz))
endef

define FIZZ_BUZZ
$(strip $(call FIZZ,$1) $(call BUZZ,$1))
endef

define TO_NUM
$(words $(subst x,x ,$1))
endef

define FB_STEP
$(or $(call FIZZ_BUZZ,$1),$(call TO_NUM,$1))
endef

define FB_RULE
fb-$(1)x: fb-$(1)
	@echo $(call FB_STEP,$1)

endef

$(eval $(foreach n,$(ALL_NUMS),$(call FB_RULE,$n)))

fb-x:
	@echo -n
```

```sh
$ make
1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
Fizz Buzz
16
17
Fizz
19
Buzz
Fizz
22
23
Fizz
Buzz
26
Fizz
28
29
Fizz Buzz
31
32
Fizz
34
Buzz
Fizz
37
38
Fizz
Buzz
41
Fizz
43
44
Fizz Buzz
46
47
Fizz
49
Buzz
Fizz
52
53
Fizz
Buzz
56
Fizz
58
59
Fizz Buzz
61
62
Fizz
64
Buzz
Fizz
67
68
Fizz
Buzz
71
Fizz
73
74
Fizz Buzz
76
77
Fizz
79
Buzz
Fizz
82
83
Fizz
Buzz
86
Fizz
88
89
Fizz Buzz
91
92
Fizz
94
Buzz
Fizz
97
98
Fizz
Buzz
```
