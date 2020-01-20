---
layout: post
title: Heapのアルゴリズムで配列のpermutationを得る
tags:
- Algorithm
- Python
---

配列のpermutationを得るための効率的なアルゴリズムとして、Heapのアルゴリズムというのが知られています。このアルゴリズムについての日本語の情報が少なかったので、内容をまとめてみました。

## アルゴリズム(再帰版)

まず最初にHeapのアルゴリズムを具体的に書き下してみます。Heapのアルゴリズムは以下のようになります:

``` python
import copy

def heap_permutation(n, array):
    """
    array の先頭 n 要素に関するすべての並び替えを返す
    """
    if n == 1:
        yield copy.copy(array)
    else:
        for i in range(0, n):
            yield from heap_permutation(n - 1, array)
            if n % 2 == 0:
                array[i], array[n - 1] = array[n - 1], array[i]
            else:
                array[0], array[n - 1] = array[n - 1], array[0]
```

……。

なんだか再帰的に `n - 1` 要素の permutation から `n` 要素の permutation を構成していそうなのはわかりますが、後半何をしているのかよくわかりませんね…？

この部分について以下で詳細を解説します。

## 解説

このアルゴリズムの骨子は、以下のようなものになります:

``` python
def heap_permutation(n, array):
    if n == 1:
        array をそのまま返す
    else:
        array のすべての要素 i について:
            i を array[n - 1] と置き換える
            heap_permutation(n - 1, array) の結果をすべて返す
```

このうち「`i` を `array[n - 1]` と置き換える」の部分の `i` の選び方をうまくやることで、 `array` のコピーを(呼び出し側に返す分以外は)作らずにすべての並び替えを行うことができるはずです。この部分がHeapのアルゴリズムのもう一つの重要な部分になります。

では、具体的にどのように `i` を選べば良いでしょうか。次のような考え方をすれば、 `i` の選び方として必要なものが見つけやすくなるかと思います:

`heap_permutation(n, array)` を実行した後、 `array[0:n-1]` の並び方は「特定の順序」になる

この性質を仮定し、その性質が再帰的にどのような `n` の場合にも成り立つようにすることで、「`i` を `array[n - 1]` と置き換える」の所で毎回どのような `i` を取ればそれがまだ末尾の要素になっていないものなのかを判断することができます。

-----

次に、上記で書いた「特定の順序」として何が適切なのかを考えていきましょう。
