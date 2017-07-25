---
layout: post
title: Metropolis-Hastings法で任意の分布の乱数を生成する
tags:
- Algorithm
- R
- use_mathjax: true
---

Mtropolis−Hastings法はマルコフ連鎖モンテカルロ法の一種。

既存の適当な乱数からほしい分布の乱数を生成する。

マルコフ連鎖モンテカルロ法自体は以下のようなコードで表すことができる。

```r
# state: 現在の状態。乱数自体も表す
# rand.g: 現在の状態を受け取り、その状態に対応する分布で乱数を返す関数
# accept: 現在の状態と rand.g(state) により生成された新しい状態を受け取り、
#         新しい状態を受容するなら TRUE, そうでなければ FALSE を返す
mcmc <- function (state, rand.g, accept) {
  while (TRUE) {
    new.state <- rand.g(state)
    if (accept(state, new.state)) {
      break
    }
  }
  new.state
}
```

棄却法と似たような感じ。生成される乱数が現在の状態に依存するところが異なる。


\\(f(x)\\): 欲しい乱数の確率密度関数(の定数倍)
\\(g(x \vert \mathrm{state})\\): fの近似としての、求まりやすい乱数の確率密度関数(パラメータとして現在の状態を取る \\(g(x \vert \mathrm{state})\\))
gは先ほどのrand.gの条件付き分布

```r
# state: 現在の状態
# f(x): 欲しい乱数の確率密度関数
# g(x | state): 適当な乱数の条件付き確率密度関数
# rand.g(state): 現在の状態に依存した分布で乱数を生成する関数
metropolis.hastings <- function (state, f, g, rand.g) {
  accept <- function (state, new.state) {
    accept.ratio <- min(1,
                        (f(new.state) * g(state, new.state)) /
                          (f(state) * g(new.state, state)))
    u <- runif(1, 0, 1)
    accept.ratio > u
  }
  mcmc(state, rand.g, accept)
}
```

実際に求める
指数分布

\\[f(x) = \frac{1}{\mu}e^{-\frac{x}{\mu}} ~ (x \leq 0)\\]

定数倍だけでいいので、

\\[f(x) = e^{-frac{x}{\mu}}\\]

としてみる

```r
# g を一様分布 U(0, 100) として、指数分布に従う乱数を求めてみる
g <- function (x, state) {
  dunif(x, 0, 100)
}

rand.g <- function (state) {
  runif(1, 0, 100)
}

mu <- 2
f <- function (x) {
  if (x < 0) 0
  else exp(-x / mu)
}

# 状態を適当な値で初期化
state <- 1

# Metropolis-Hastings法で乱数を一つ生成
myrexp <- function () {
  new.state <- metropolis.hastings(state, f, g, rand.g)
  assign("state", new.state, env = .GlobalEnv)
  new.state
}
```

これで試しに乱数を10000個生成して、正しい指数分布と比較してみる

```r
# g を一様分布 U(0, 100) として、指数分布に従う乱数を求めてみる
g <- function (x, state) {
  dunif(x, 0, 100)
}

rand.g <- function (state) {
  runif(1, 0, 100)
}

mu <- 2
f <- function (x) {
  if (x < 0) 0
  else exp(-x / mu)
}

# 状態を適当な値で初期化
state <- 1

# Metropolis-Hastings法で乱数を一つ生成
myrexp <- function () {
  new.state <- metropolis.hastings(state, f, g, rand.g)
  assign("state", new.state, env = .GlobalEnv)
  new.state
}

N <- 10000
df = data.frame(exp1 = rexp(N, 1 / mu))

# 最初の方に生成される乱数は初期値への依存度が高いので、最初の10000件を捨てる
df$exp2 <- sapply(df$exp1, function (x) { myrexp() })

df$exp2 <- sapply(df$exp1, function (x) { myrexp() })

df.plot <- df %>% gather()
ggplot(df.plot, aes(x = value, fill = key)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.5)
```

![/img/post/2017-07-25-matropolis-hastings.png](/img/post/2017-07-25-matropolis-hastings.png)

あまり一致度は高くない。

gが真の分布fに近いほど精度は高くなる

今回は指数分布を一様分布で近似しようとしたので、精度は悪くてもまあ仕方なさそう
