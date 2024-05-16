# Protobuf definition and generated Haskell modules

To re-generate the Haskell modules, run

```
protoc \
  --plugin=protoc-gen-haskell=`which proto-lens-protoc` \
  --haskell_out=proto \
  --proto_path=proto proto/kvstore.proto
```

You will need to have [`protoc`](https://grpc.io/docs/protoc-installation/)
installed (part of the gRPC), as well as the corresponding Haskell plugin
[`proto-lens-protoc`](https://hackage.haskell.org/package/proto-lens-protoc).

The generated Haskell depends on the
[`proto-lens-runtime`](https://hackage.haskell.org/package/proto-lens-runtime)
package.