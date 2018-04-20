---
layout: post
title: M-Set
tags:
- 圏論
use_mathjax: true
---

## モノイドの圏
対象は一つ。射は複数存在しうる。この射が通常の意味でモノイド。

## 関手
圏 \\(\mathcal{C}\\) から圏 \\(\mathcal{D}\\) への関手は以下のようなもの。

* \\(\mathcal{C}\\) の各対象 \\(X\\) を \\(\mathcal{D}\\) の対象 \\(F(X)\\) に移す
* \\(\mathcal{C}\\) の各射 \\(f : X \to Y\\) を \\(\mathcal{D}\\) の射 \\(F(f) : F(X) \to F(Y)\\) に移す
* \\(\mathcal{C}\\) の各対象 \\(X\\) について、 \\(F(\mathrm{id}_X) = \mathrm{id}_{F(X)}\\)
* \\(\mathcal{C}\\) の各射 \\(f : X \to Y, g : Y \to Z\\) について、 \\(F(g \circ f) = F(g) \circ F(f)\\)


## M-set
モノイドの圏 \\(\mathcal{M}\\) に対して、以下のような \\(\mathcal{M} \to \mathcal{Set}\\) の関手を M-set という。

1. \\(\mathcal{M}\\) の唯一の対象は、ある集合 \\(S\\) に写される。
2. \\(m \in \mathrm{Arr}(\mathcal{M})\\) は \\(S \to S\\) の関数に写される。

2で写される関数を、 \\(S\\) 上における \\(m\\) のアクションという。

### 例: 有限オートマトン
例えば文字集合 \\(\Sigma\\), 状態集合 \\(S\\), 遷移関数 \\(\delta : \Sigma \times S \to S\\) の有限オートマトンを考える。

オートマトンに入力する文字列 \\(\Sigma^*\\) は空文字列 \\(\epsilon\\) を単位元とするモノイド。このモノイドを上で説明したモノイドの圏と同一視する。

以下のような \\(\Sigma^*\\)-set \\(F : \Sigma^* \to S\\) を考える。

\\(x \in \mathrm{Arr}(\Sigma^*), s \in S\\) に対して、 \\(F(x)(s) = \delta(x, s)\\)

ただし \\(\delta\\) は定義域を \\(\Sigma^*\\) へ拡張したもの。

## 参考

[Databases are categories](http://math.mit.edu/~dspivak/informatics/talks/galois.pdf)
[CTDBIntroductoryTalk](http://math.mit.edu/~dspivak/informatics/talks/CTDBIntroductoryTalk)
