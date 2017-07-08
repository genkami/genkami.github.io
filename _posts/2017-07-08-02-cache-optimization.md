---
layout: post
title: CPUのキャッシュを利用して4倍速い行列積を書く
tags:
- C
---

まずは何の変哲もない行列積を求めるコードを御覧ください。

``` c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define N 500
#define M 500

// N x M 行列を作る
double **new_matrix() {
  double **mat = malloc(sizeof(double *) * N);
  for (int i = 0; i < N; i++) {
    mat[i] = malloc(sizeof(double) * M);
  }
  return mat;
}

void zerofill(double **mat) {
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      mat[i][j] = 0;
    }
  }
}

// c = ab
void multiply(double **a, double **b, double **c) {
  zerofill(c);
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      for (int k = 0; k < N; k++) {
        c[i][j] += a[i][k] + b[k][j];
      }
    }
  }
}

int main() {
  double **a = new_matrix();
  double **b = new_matrix();
  double **c = new_matrix();
  srand(time(NULL));
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < M; j++) {
      a[i][j] = (double)rand();
      b[i][j] = (double)rand();
    }
  }
  multiply(a, b, c);
}
```

これを実行してみましょう。

```
$ gcc -o matrix_a matrix_a.c
$ time ./matrix_a

real	0m1.662s
user	0m1.650s
sys	0m0.007s
```

次に、このコードを少しだけ変えてみます。

とは言っても、`multiply`のループの順番をi→j→kからi→k→jにしただけで、大したことはしていません。

``` c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define N 500
#define M 500

// N x M 行列を作る
double **new_matrix() {
  double **mat = malloc(sizeof(double *) * N);
  for (int i = 0; i < N; i++) {
    mat[i] = malloc(sizeof(double) * M);
  }
  return mat;
}

void zerofill(double **mat) {
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      mat[i][j] = 0;
    }
  }
}

// c = ab
void multiply(double **a, double **b, double **c) {
  zerofill(c);
  for (int i = 0; i < N; i++) {
    for (int k = 0; k < N; k++) {
      for (int j = 0; j < N; j++) {
        c[i][j] += a[i][k] + b[k][j];
      }
    }
  }
}

int main() {
  double **a = new_matrix();
  double **b = new_matrix();
  double **c = new_matrix();
  srand(time(NULL));
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < M; j++) {
      a[i][j] = (double)rand();
      b[i][j] = (double)rand();
    }
  }
  multiply(a, b, c);
}
```

これを実行してみます。

```
$ gcc -o matrix_b matrix_b.c
$ time ./matrix_b

real	0m0.405s
user	0m0.398s
sys	0m0.005s
```

なんと、元のコードに対して4倍近く早くなっていることがわかります。

この原因は、キャッシュが効率的に働いているかどうかです。

上のコードでの行列の実装では、各`i`に対して`a[i]`はそれぞれ別な場所にアロケートされています。

一般的に、キャッシュはメモリアクセスの空間的局所性を利用しようとするので、あるアドレスの値が読み込まれた場合、その近くのアドレスのデータも一緒にキャッシュに持ってくる性質があります。

そのため、i→j→kのループだと、一番内側のループごとに`b[k][j]`の参照でキャッシュミスが(高確率で)起こってしまいます。一方、i→k→jのループだと、一番内側のループでは、高確率で前回のループで参照した位置の隣を指します。そのため、ループごとのキャッシュミスが起こる頻度が低くなり、高速で行列の要素にアクセスできるようになっているのです。
