---
layout: post
title: 
tags:
- Ruby
---

bundler 1.16.1
pre-commit 0.27.0

`PreCommit.run`

``` ruby
/path/to/ruby/gems/2.2.0/gems/bundler-1.16.1/lib/bundler/stub_specification.rb:43:in `activated': undefined method `activated' for #<Gem::StubSpecification:0x007fc99a87acd8> (NoMethodError)
```

bundler 1.14.4 にしたら動いた

bundlerのバージョンが原因

``` sh
$ git diff v1.16.1..v1.14.4 lib/bundler/stub_specification.rb
-
-    def activated
-      stub.activated
-    end
-
```

`activated` メソッドが消されたのが原因らしい


もう少し詳しく調べてみる

``` sh
$ git blame lib/bundler/stub_specification.rb
...
3e2ac90ca0 (Samuel Giddins    2017-02-18 20:18:26 -0800  41)
8b95a4858e (Samuel Giddins    2017-04-06 18:56:26 -0500  42)     def activated
8b95a4858e (Samuel Giddins    2017-04-06 18:56:26 -0500  43)       stub.activated
8b95a4858e (Samuel Giddins    2017-04-06 18:56:26 -0500  44)     end
8b95a4858e (Samuel Giddins    2017-04-06 18:56:26 -0500  45)
...
```

``` sh
$ git log 8b95a4858e
commit 8b95a4858e15cf667c70db8c4d9c0333a28b55e8
Author: Samuel Giddins <segiddins@segiddins.me>
Date:   Thu Apr 6 18:56:26 2017 -0500

    [StubSpecification] Avoid loading the full spec when possible

...
```

[#5568](https://github.com/bundler/bundler/pull/5568)


