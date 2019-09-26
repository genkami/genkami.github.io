---
layout: post
title: StackProfのdumpファイルをspeedscopeで見る
tags:
- Ruby
---

speedscopeはJSONにしか対応してませんが、StackProfがデフォルトでMarshalで保存してしまうので自力でJSONに変換する必要があったりします。


幸い中身の構造は一緒なので単に `Marshal` でロードして `JSON` でダンプすればいいだけなので、こんな感じのRakeタスクとかを用意しておくとちょっと楽です。

```ruby
require "json"

namespace :stackprof do
  task :dump_to_json, [:path] do |_, args|
    in_path = args.path
    out_path = in_path.gsub(/\.dump$/, ".json")
    prof = Marshal.load(IO.binread(in_path))
    File.write(out_path, JSON.generate(prof))
  end
end
```

使い方:

まずは普通にプロファイルを取る。今回は例としてFizz Buzz。

``` ruby
require "stackprof"

def print_fizzbuzz
  fizzbuzz(1, 100).each { |msg| puts msg }
end

def fizzbuzz(start, end_)
  (start..end_).map { |n| _fizzbuzz(n) }
end

def _fizzbuzz(n)
  if n % 15 == 0
    "Fizz Buzz"
  elsif n % 3 == 0
    "Fizz"
  elsif n % 5 == 0
    "Buzz"
  else
    n.to_s
  end
end

StackProf.run(mode: :cpu, raw: true, out: "stackprof.dump", interval: 100) do
  10000.times do
    print_fizzbuzz
  end
end
```

普通に実行したあと `.dump` ファイルをJSONに変換する

```
$ bundle exec ruby fizzbuzz.rb
$ bundle exec rake 'stackprof:dump_to_json[stackprof.dump]'
```

speedscopeで開く

```
$ speedscope stackprof.json
```


![/img/post/2019-09-26-speedscope.png](/img/post/2019-09-26-speedscope.png)
