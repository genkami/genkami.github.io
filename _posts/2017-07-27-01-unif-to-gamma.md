---
layout: post
title: 棄却法を使ってガンマ分布を作る
tags:
- Algorithm
- R
use_mathjax: true
---

## 棄却法

棄却法は、確率密度関数\\(f(x)\\)に従う確率変数を、一様分布の確率変数\\(U \sim U(0,  1)\\)と都合のいいの確率密度関数\\(g(v)\\)に従う確率変数\\(V\\)を使って求めるものです。

\\(g(v)\\)に従う確率変数\\(V\\)は生成できるものとして、さらにある定数\\(c\\)について以下の式が成り立つと仮定します。

\\[f(x) \leq cg(x), \forall x\\]

このような\\(c\\)が存在するとき、これを用いて以下の関数を定義します。

\\[\omega(v) = \frac{f(v)}{cg(v)}\\]

この\\(\omega\\)を用いた棄却法というアルゴリズムで\\(f\\)に従う確率変数を生成します。

棄却法は、以下のようなアルゴリズムになっています

1. 一様乱数\\(u \sim U(0, 1)\\)を生成する。
2. 確率密度関数が\\(g(v)\\)で表されるような乱数\\(v\\)を生成する。
3. もし、\\(u \leq \omega(v)\\) であれば、\\(v\\) を結果の値とする。
4. そうでなければ、1からやり直す。

このアルゴリズムで生成される乱数は、確率密度関数が\\(f(x)\\)である分布に従うことが知られています。

一見意味がわからない操作ですが、実際に計算してみるとちゃんと出力の分布が\\(f\\)に従っていることがわかります。

示すべきは、

\\[P(a \leq V \leq b \vert U \leq \omega(V)) = \int_{a}^{b}f(v)\mathrm{d}v\\]

です。これは簡単に計算することができます。

\\[P(a \leq V \leq b \vert U \leq \omega(V))\\]
\\[=\frac{P(a \leq V \leq b \cap U \leq \omega(V))}{P(U \leq \omega(V))}\\]
\\[=\frac{\int_{a}^{b}\int_{0}^{\omega(v)}g(v)\times 1\mathrm{d}u\mathrm{d}v}{\int_{-\infty}^{\infty}\int_{0}^{\omega(v)}g(v)\times 1\mathrm{d}u\mathrm{d}v}\\]
\\[=\frac{\int_{a}^{b}g(v)\int_{0}^{\omega(v)}\mathrm{d}u\mathrm{d}v}{\int_{-\infty}^{\infty}g(v)\int_{0}^{\omega(v)}\mathrm{d}u\mathrm{d}v}\\]
\\[=\frac{\int_{a}^{b}g(v)\omega(v)\mathrm{d}v}{\int_{-\infty}^{\infty}g(v)\omega(v)\mathrm{d}v}\\]
\\[=\frac{\int_{a}^{b}\frac{f(v)}{c}\mathrm{d}v}{\int_{-\infty}^{\infty}\frac{f(v)}{c}\mathrm{d}v}\\]
\\[=\frac{\int_{a}^{b}f(v)\mathrm{d}v}{\int_{-\infty}^{\infty}f(v)\mathrm{d}v}\\]
\\[=\int_{a}^{b}f(v)\mathrm{d}v\\]

というわけで、棄却されなかった場合の\\(V\\)の分布が\\(g\\)によらずに\\(f\\)に従っていることがわかりました。


## ガンマ分布
ガンマ分布とは、ガンマ関数

\\[\Gamma(k) = \int_0^{\infty}x^{k-1}e^{-x}\mathrm{d}x\\]

を用いた分布のことです。

形状パラメータ\\(k > 0\\), スケールパラメータ\\(\theta > 0\\)のガンマ分布\\(\mathrm{Gamma}(k, \theta)\\)は、以下の確率密度関数で表されます。

\\[P_{\mathrm{Gamma}(k, \theta)}(x) = \frac{x^{k-1}}{\Gamma(k)\theta^k}e^{-frac{x}{\theta}}\\]

今回は簡単のため、\\(\theta = 1\\)の場合のみ考えます。実はこの場合のみ考えれば十分なことが後からわかります。

棄却法を用いてガンマ分布\\(\mathrm{Gamma}(k,1)\\)に従う乱数を生成するために、適当な確率密度関数\\(g(x)\\)と定数\\(c\\)を定義します。\\(g\\)として有名なものは、Ahrens-Dieterの方法による以下の定義などがあります。今回はこの定義を採用します。

\\[cg(x) = \frac{x^{k-1}}{\Gamma(k)} ~ (\mathrm{if}~0 \leq x \leq 1)\\]
\\[cg(x) = \frac{e^{-x}}{\Gamma(k)} ~ (\mathrm{otherwise})\\]

ところで、\\(g\\)は確率密度関数なので、以下を満たさなければなりません。

\\[\int_{0}^{\infty}g(x)\mathrm{d}x = 1\\]

計算すると、

\\(0 \leq x \leq 1\\)のとき、
\\[\int_{0}^{x}g(x)\mathrm{d}x = \frac{x^k}{ck\Gamma(k)}\\]

\\(1 < x\\)のとき、
\\[\int_{0}^{x}g(x)\mathrm{d}x = \int_{0}^{1}g(x)\mathrm{d}x + \int_{1}^{x}g(x)\mathrm{d}x = \frac{e+k-ke^{1-x}}{cke\Gamma(k)}\\]

であることがわかるので、

\\[\int_{0}^{\infty}g(x)\mathrm{d}x = \frac{e+k}{cke\Gamma(k)} = 1\\]

したがって、

\\[c = \frac{e + k}{ke\Gamma(k)}\\]

となります。

この確率密度関数\\(g(x)\\)に従う確率変数は、[逆関数法](/2017/07/26/01-unif-to-exp.html)で作ることができます。

\\(g\\)の累積分布関数を\\(G\\)とすると、先ほどの結果に\\(c\\)を代入して

\\[G(x) = \frac{x^k}{ck\Gamma(k)} = \frac{ex^k}{e + k} ~ (\mathrm{if} 0 \leq x \leq 1)\\]
\\[G(x) = \frac{e + k - ke^{1-x}}{cke\Gamma(k)} = 1 - \frac{ke^{1-x}}{e + k} ~ (\mathrm{otherwise})\\]

したがって、\\(y = G(x)\\)とすると、\\(0 \leq y \leq \frac{e}{e+k}\\)のとき

\\[y = \frac{e}{e + k}x^k\\]
\\[x^k = \frac{e + k}{e}y\\]
\\[\therefore x = \left( \frac{e + k}{e}y \right)\\]

また、\\(\frac{e}{e+k} < y\\)のとき、

\\[y = 1 - \frac{ke^{1-x}}{e + k}\\]
\\[e^{1-x} = \frac{e + k}{k}(1 - y)\\]
\\[1 - x = \log \frac{(e + k)(1 - y)}{k}\\]
\\[\therefore x = -\log \frac{(e + k)(1 - y)}{ke}\\]

これで逆関数が求まりました。あとは一様乱数\\(U^{\prime} \sim U(0, 1)\\)に対し、
\\(V = G^{-1}(U^{\prime})\\)として、先ほどのアルゴリズムに突っ込めば完成です。

実際に、上の方法に従ってガンマ分布の乱数を生成するコードを書いてみました。

```r
# 棄却法
# omega は先ほどの ω
# rand.g は g に従う乱数を生成する関数
rejection.sampling <- function (omega, rand.g) {
  u <- runif(1, 0, 1)
  v <- rand.g()
  if (u <= omega(v)) v else rejection.sampling(omega, rand.g)
}

k <- 1
e <- exp(1)

# Gの逆関数
g.inv <- function (y) {
  if (0 <= y && y <= e/ (e + k)) {
    ((e + k) * y / e) ^ (1 / k)
  } else {
    -log((e + k) * (1 - y) / (k * e))
  }
}

# Ahrens-Dieter の方法
ahrens.dieter <- function () {
  omega <- function (x) {
    if (0 <= x && x <= 1) {
      exp(-x)
    } else {
      x^(k - 1)
    }
  }
  rand.g <- function () {
    g.inv(runif(1, 0, 1))
  }
  rejection.sampling(omega, rand.g)
}
```

実際にグラフを書いてみました。

```r
N <- 100000
df = data.frame(gamma1 = rgamma(N, shape = k, scale = 1))
df$gamma2 <- sapply(df$gamma1, function (x) { ahrens.dieter() })

df.plot <- df %>% gather()
ggplot(df.plot, aes(x = value, fill = key)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.5)
```

![/img/post/2017-07-23-gamma-01.png](/img/post/2017-07-23-gamma-01.png)

`gamma1`が`rgamma()`によって生成されたガンマ分布の乱数で、`gamma2`は自作のものです。

両者はほとんど一致していることがわかります。

ちなみに、\\(X \sim \mathrm{Gamma}(k, 1)\\) のとき、\\(\theta X \sim \mathrm{Gamma}(k, \theta)\\)なので、\\(\theta = 1\\)の場合さえできてしまえば他のスケールの乱数も簡単に生成することができます。

以下は\\(\theta = 0.3\\)の例です。

```r
N <- 100000
df = data.frame(gamma1 = rgamma(N, shape = k, scale = 0.3))
df$gamma2 <- sapply(df$gamma1, function (x) { 0.3 * ahrens.dieter() })

df.plot <- df %>% gather()
ggplot(df.plot, aes(x = value, fill = key)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.5)
```

![/img/post/2017-07-23-gamma-02.png](/img/post/2017-07-23-gamma-02.png)

こちらも一致していることがわかります。
