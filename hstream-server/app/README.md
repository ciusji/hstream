
# hstream-server

## Introduction

本项目旨在快速原型 hstream-server.

## Build


首先从 .proto 生成 .hs 

**每次如果修改了 .proto 文件，都需要运行下面的命令重新生成对应的 .hs 文件**

```bash

cd hstream-server

compile-proto-file --includeDir ./app --proto HStreamApi.proto --out ./app

```

然后用 cabal build


```bash

cabal build --extra-lib-dirs=/home/wangbin/tmp/grpc-1.35.0-install/lib 

```

最后执行 hstream-server 和 hstream-client 

```bash

## 启动 hstream-server

cabal run --extra-lib-dirs=/home/wangbin/tmp/grpc-1.35.0-install/lib hstream-server 

## 启动 hstream-client-example

cabal run --extra-lib-dirs=/home/wangbin/tmp/grpc-1.35.0-install/lib hstream-client-example

```
