
## Prerequisites

To build LING from sources, the following prerequisites must be met:

1. Erlang/OTP

   You must have [Erlang/OTP release 17 installed](https://www.erlang-solutions.com/downloads/download-erlang-otp).

2. When using OS X, you need to have a [cross-compiling toolchain](http://crossgcc.rts-software.org/doku.php?id=compiling_for_linux)
   installed on your system. LING has been tested to build against GCC 4.8.1 and 4.9.

## Build

```
make
make install
```

Make sure to run `git clean -dxf` to start over if you see weird error
messages you can't easily explain.
