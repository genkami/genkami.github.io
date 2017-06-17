---
layout: post
title: RRB-Treeによる配列の実装
tags:
- Elm
use_mathjax: true
---

Haskellは効率的で実質immutableな配列を扱うためにDiffArrayを使う

[破壊的変更を隠蔽するDiffArray - genkami.github.io](/2017/06/17/haskell-diffarray.html)

ではIOモナドすらないElmではどのようにして配列が実装されているか

[Elm 0.12.1 Fast, Immutable Arrays](http://elm-lang.org/blog/announce/0.12.1)

によると、Elm 0.12.1移行はRRB-Treeを使っているらしい。

RRB-Treeの元論文: [RRB-Trees: Efficient Immutable Vectors](https://infoscience.epfl.ch/record/169879/files/RMTrees.pdf)

RRB-TreeはN分木(ElmではN=32) \\(O(\mathrm{log}n)\\)


[前の記事のプログラム](/2017/06/17/haskell-diffarray.html)のElm版

``` haskell
module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time)
import Array exposing (Array)
import Random
import Task

type alias Model =
    { array : Array.Array Int
    , startTime : Time
    , endTime : Time
    }

type Msg = Start Time
         | RandomNums (List Int)
         | End Time

main =
    program { init = init
            , view = view
            , update = update
            , subscriptions = subscriptions
            }

size : Int
size = 100000
init : (Model, Cmd Msg)
init = ({ array = Array.repeat size 0
        , startTime = 0
        , endTime = 0
        }, initCmd)

initCmd : Cmd Msg
initCmd = Task.perform Start Time.now

randomVals : Int -> Int -> Int -> Random.Generator (List Int)
randomVals n min max = Random.list n (Random.int min max)

writeVals : Array a -> List a -> Array a
writeVals arr xs =
    let write n arr xs =
            case xs of
                [] -> arr
                y :: ys -> write (n + 1) (Array.set n y arr) ys
    in write 0 arr xs

view : Model -> Html Msg
view model =
    div [] [
          p [] [
               text (toString (Array.foldl (+) 0 model.array))
              ]
        , p [] [
                text "time: "
              , text ((toString ((model.endTime - model.startTime) / Time.second)) ++ "sec")
              ]
        ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Start time -> ({ model | startTime = time
                       }, Random.generate RandomNums (randomVals size 0 100))
        RandomNums nums -> ({ model | array = writeVals model.array nums }
                           , Task.perform End Time.now)
        End time -> ({ model | endTime = time }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none
```

実行結果
![実行結果](/img/post/2017-06-17-elm-array-result.png)

0.216sec

Elmなので遅いけど、それでもHaskellの`Array`のバージョンよりは速い。
