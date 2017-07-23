---
layout: post
title: 逆関数法で一様分布から指数分布を作る
tags:
- Algorithm
- R
---

\\[U \~ U(0, 1)\\]
\\[f_{U(a, b)}(x) = 1 ~ (a \leq x \leq b)\\]
\\[f_{U(a, b)}(x) = 0 ~ (\mathrm{otherwise})\\]

\\[X \~ Ex(\lambda)\\]
\\[f_{Ex(\lambda)}(x) = \lambda e^{-\lambda x}\\]

\\[F(x) = P(x \leq X) = \int_{0}^{x}f_{Ex(\lambda)}(x)\mathrm{d}x\\]

\\[u = F(x)\\]
\\[u = [-e^{-\lambda x}]_{0}^{x}\\]
\\[= -\frac{\log (1 - u)}{\lambda}\\]

\\[U \~ U(0, 1)\\]
\\[X = F^{-1}(U)\\]

が指数分布

```r
library(ggplot2)
library(tidyr)
library(dplyr)

N <- 10000
df <- data.frame(unif = runif(N, min = 0, max = 1)) # 一様乱数

lambda <- 1
df$exp1 <- rexp(N, rate = lambda) # 指数分布(標準)
df$exp2 <- -log(1 - df$unif) / lambda # 指数分布(自作)

df.plot <- df %>% select(one_of(c("exp1", "exp2"))) %>% gather()
ggplot(df.plot, aes(x = value, fill = key))
  + geom_histogram(bins = 10, boundary = 1, position = "identity", alpha = 0.5)
```

![/img/post/2017-07-22-rexp-hist.png](/img/post/2017-07-22-rexp-hist.png)

`exp1`と`exp2`の2つのヒストグラムがほぼ一致している


これは指数分布だけでなく、逆関数が求められる任意の分布に対して使うことができる。
