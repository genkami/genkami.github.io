---
layout: post
title: 以外と知らないGCCの拡張文法
tags:
- C
---

## 文を式として扱う
複文を括弧で括ることによって、その複文を式として扱える。最後の文の実行結果がそのままその式の値になる。Rubyとかみたいな感じの挙動。

```c
int main() {
     for (int i = 1; i <= 100; i++) {
          char msg[10];
          printf("%s\n", ({
                         if (i % 15 == 0) strcpy(msg, "Fizz Buzz");
                         else if (i % 3 == 0) strcpy(msg, "Fizz");
                         else if (i % 5 == 0) strcpy(msg, "Buzz");
                         else sprintf(msg, "%d", i);
                         msg;
                    }));
     }
} //=> 1 2 Fizz 4 Buzz Fizz ...
```

## ラベルを値として扱う
ラベルを`void *`の値として扱うことができます。ラベルの指すアドレスは`&&`によって取得することができます。

```c
int main() {
     void *lab = &&LABEL;
     goto *lab;
     return 0;
LABEL:
     printf("heyhey!\n");
     return 0;
} //=> heyhey!
```

## Typeof 演算子
`typeof(a)`で式`a`の型を取得することができます。たぶんC++の`decltype`に近いもののはず。C++知らないけど。

```c
#define MAX(a, b)                               \
     ({                                         \
          typeof(a) _a = (a);                   \
          typeof(b) _b = (b);                   \
          _a > _b ? _a : _b;                    \
     })
int main() {
     printf("MAX(5, 3): %d\n", MAX(5, 3)); //=> 5
     return 0;
}
```

## __auto_type
`typeof`よりもう一段階便利な機能。`__auto_type`を使うことで、処理系が型推論をしてくれるようになります。

```c
#define MAX(a, b)                                   \
     ({                                             \
          __auto_type _a = (a);                     \
          __auto_type _b = (b);                     \
          _a > _b ? _a : _b;                        \
     })
int main() {
     printf("MAX(5, 3): %d\n", MAX(5, 3)); //=> 5
     return 0;
}
```

## 三項演算子の二項目の省略
`x ? : y`が`x ? x : y`に近い挙動をするようになります。ただし、`x`は一度しか評価されません。

## 可変長引数マクロ
マクロも可変長引数を受け取ることができます。

完全な拡張文法のリストは[公式のドキュメント](https://gcc.gnu.org/onlinedocs/gcc/C-Extensions.html)を参照してください。

## caseで範囲にマッチ
