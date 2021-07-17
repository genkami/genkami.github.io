---
layout: post
title: gRPC でメソッドが副作用を持つ場合冪等性キーを持つことを保証する
tags:
- Go
- Protocol Buffers
- gRPC
---

gRPC に限らず、一般的に副作用のある API 呼び出しを行う際にはリクエストに一意な ID (いわゆる冪等性キー)を付け、同じ ID のリクエストが複数回飛んできた場合後続のものは無視することによって冪等性を保つ手法が取られることがあります。

このような冪等性キーを用いる方法は gRPC の仕様として標準化されてはいないものの、特定のサービス内や組織内で統一した方法を強制したい場合は多いかと思います。この記事では、 gRPC の Go 実装においてそのような冪等性キーに関するルールを強制する方法を紹介します。

## IdempotencyLevel MethodOption
gRPC には [IdempotencyLevel](https://pkg.go.dev/google.golang.org/protobuf@v1.27.1/types/descriptorpb#MethodOptions_IdempotencyLevel) という `MethodOption` が存在します。 `IdempotencyLevel` の値は `IDEMPOTENCY_UNKNOWN`, `NO_SIDE_EFFECTS`, `IDEMPOTENT` の3種類があり、これらを指定することでメソッドの冪等性のレベルを注釈することができます:

```protobuf
service Example {
    rpc ReadOnlyMethod(DummyReadOnlyRequest) returns (DummyReadOnlyResponse) {
        option idempotency_level = NO_SIDE_EFFECTS;
    }

    rpc ReadWriteMethod(DummyReadWriteRequest) returns (DummyReadWriteResponse) {
        option idempotency_level = IDEMPOTENT;
    }
}
```

ただし、これらはあくまで注釈を行うことができるというだけで、これらのオプションの具体的な使い方は規定されていないようです。実際、 [golang/protobuf](https://github.com/golang/protobuf) や [grpc/grpc-go](https://github.com/grpc/grpc-go) のコードを見ても、これらのオプションについては型が定義されているのみで、実際になにかに使われているわけではありません。

## 設定された MethodOption を読む
少なくとも grpc-go では、 [`FileDescriptor`](https://pkg.go.dev/google.golang.org/protobuf@v1.27.1/reflect/protoreflect#FileDescriptor) を通して `MethodOption` のようなメタデータを簡単に取得することができます。
例えば、上記の protobuf から生成された `.pb.go` のパッケージ名を `pb` として、サービス `Example` のメソッド `ReadOnlyMethod` のオプション `idempotency_level` を取得する方法は次のようになります:

```go
import "google.golang.org/protobuf/types/descriptorpb"
...

fileDesc := pb.File_api_api_proto // 具体的な名前はファイル名やパッケージ名によって異なります
methodDesc := fileDesc.Services().ByName("Example").Methods().ByName("ReadOnlyMethod")
methodOpts := methodDesc.Options().(*descriptorpb.MethodOptions)
// nil チェック等は省略しています
switch *methodOpts.IdempotencyLevel {
case descriptorpb.MethodOptions_IDEMPOTENCY_UNKNOWN:
	fmt.Println("UNKNOWN")
case descriptorpb.MethodOptions_NO_SIDE_EFFECTS:
	fmt.Println("NO_SIDE_EFFECTS")
case descriptorpb.MethodOptions_IDEMPOTENT:
	fmt.Println("IDEMPOTENT")
}
```

これを利用することで、 `IdempotencyLevel` が `NO_SIDE_EFFECTS` でないメソッドに対して冪等性キーを持たせることをテストで強制することができます。

## リクエストに冪等性キーが含まれることをテストする
先程のオプションと同様に、メソッドのリクエストに含まれるフィールドなどの情報についても `Descriptor` から取得することができます。
例えば、以下の例ではサービス `Example` のメソッド `ReadOnlyMethod` がリクエストに `idempotency_key` というフィールドを持っているかどうかを調べています:

```go
fileDesc := pb.File_api_api_proto
methodDesc := fileDesc.Services().ByName("Example").Methods().ByName("ReadOnlyMethod")
fieldDesc := methodDesc.Input().Fields().ByName("idempotency_key")
if fieldDesc != nil {
	fmt.Printf("method %s has idempotency_key\n", methodDesc.Name())
} else {
	fmt.Printf("method %s does not have idempotency_key\n", methodDesc.Name())
}
```

これら2つの例を組み合わせることで、最終的に `NO_SIDE_EFFECTS` でないメソッドが `idempotency_key` を持つことをテストによって強制することができます:

```go
func TestApi(t *testing.T) {
	fileDesc := pb.File_api_api_proto
	// 今回は簡単のために Examples サービスのみを対象としています。
	methodDescs := fileDesc.Services().ByName("Example").Methods()
	for i := 0; i < methodDescs.Len(); i++ {
		methodDesc := methodDescs.Get(i)
		opts := methodDesc.Options().(*descriptorpb.MethodOptions)
		var hasSideEffects bool
		if opts.IdempotencyLevel == nil {
			hasSideEffects = true
		} else {
			hasSideEffects = *opts.IdempotencyLevel != descriptorpb.MethodOptions_NO_SIDE_EFFECTS
		}

		if hasSideEffects {
			idempotencyKeyDesc := methodDesc.Input().Fields().ByName("idempotency_key")
			if idempotencyKeyDesc == nil {
				t.Errorf("%s: non-readonly method must have idempotency_key", methodDesc.Name())
			}
		}
	}
}
```

完全なコードの例は [こちら](https://github.com/genkami/examples/tree/main/go/protobuf/ensure-idempotent/) に置いてあります。
