
# hstream-server

## Introduction

本项目旨在快速原型 hstream-server.

## Build


首先从 .proto 生成 .hs 

**每次如果修改了 .proto 文件，都需要运行下面的命令重新生成对应的 .hs 文件**

```bash

cd hstream-server

compile-proto-file --includeDir ./proto --proto hstream/server/HStreamApi.proto --out ./generated-src

```

然后用 cabal build

**注意：** 如果 grpc 安装在非系统默认路径，需要首先设置 `LD_LIBRARY_PATH`，如下：

``` bash

export LD_LIBRARY_PATH=/home/wangbin/tmp/grpc-1.35.0-install/lib:$LD_LIBRARY_PATH

```

并在运行 cabal 命令的时候指定 `--extra-lib-dirs`:

```bash

## cabal build --extra-lib-dirs=/home/wangbin/tmp/grpc-1.35.0-install/lib 

cabal build --extra-lib-dirs=/usr/local/lib

```

最后执行 hstream-server 和 hstream-client 

```bash

## 启动 hstream-server

# cabal run --extra-lib-dirs=/home/wangbin/tmp/grpc-1.35.0-install/lib hstream-server 

cabal run --extra-lib-dirs=/usr/local/lib hstream-server 

## 启动 hstream-client-example

## cabal run --extra-lib-dirs=/home/wangbin/tmp/grpc-1.35.0-install/lib hstream-client-example

cabal run --extra-lib-dirs=/usr/local/lib hstream-client-example

```

也可以通过在 `~/.cabal/config` 配置文件里面指定 `--extra-lib-dirs`，这样 cabal 命令在运行的时候就无需指定这个参数了。
