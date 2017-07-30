---
layout: post
title: 
tags:
- 
---

編集距離のindexingについて

http://www.sciencedirect.com/science/article/pii/0022000080900021
A faster algorithm computing string edit distances
1980
n >= m としてn, m文字の文字列間の編集距離を O(n*max(1, m/logn)) で行えるらしい

http://web.cs.ucla.edu/~rafail/PUBLIC/68.pdf
Low Distortion Embeddings for Edit Distance
2005 って書いてるけど、たぶん新しいバージョンが 2007 に出てるので探す
approximation factor 2^{O(\sqrt{\log d \log \log d})}
{0, 1}^d の比較なのでちょっと近いかも
ただぱっとabstract読んだだけではあんまり何やってるかわからなかった
l_1 への埋め込みってなんだ？？ l_1 って何？？

https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/vldb06.pdf
2006
類似度の高い集合を探す方法らしい
{0, 1}^d は集合と同一視できるのでもしかしたら有用かも

http://www.mit.edu/~andoni/papers/compEdit.pdf
APPROXIMATING EDIT DISTANCE IN NEAR-LINEAR TIME
2009
n文字の文字列同士の編集距離の概算をn^{1 + o(1)}で行えるらしい
approximation factor(これどうやって日本語にするの) 2^{O(\sqrt{\log n})}

http://www.mit.edu/~andoni/papers/editQuery-focs.pdf
Polylogarithmic Approximation for Edit Distance and the Asymmetric Query Complexity
2010
approximation factor (\log n)^{O(1/\epsilon)} in n^{1 + \epsilon} time

https://www.comp.nus.edu.sg/~ooibc/sigmod10bedtree.pdf
2010
B+-tree の亜種を使って編集距離を含む文字列間の類似度を測るためのインデックスを作れるらしい

https://arxiv.org/abs/1408.0467
Online Pattern Matching for String Edit Distance with Moves
2014
EDM(Edit Distance with Moves)のsuccinctなindex

https://link.springer.com/chapter/10.1007/978-3-319-11988-5_11
A Compressed Index for Hamming Distances
2014
LSHのsuccinct version?
まずはLSHから調べないと

http://www.sciencedirect.com/science/article/pii/S1570866712001128
ESP-index: A compressed index based on edit-sensitive parsing
2016
これもEDMのsuiinctなindex

similarity joins: 似たようなやつのペアを列挙するということらしい
