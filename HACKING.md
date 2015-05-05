
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

## Development using Vagrant

To begin, make sure [Vagrant](https://www.vagrantup.com) is available on your system.

Quick start:

```bash
vagrant up
vagrant reload # installs a new kernel
```

You can follow these additional steps to improve your workflow:

```bash
# Prepare your ssh client
vagrant ssh-config --host hackling >> ~/.ssh/config # prepares your ssh client

# Create a remote in your local git working dir to push to Vagrant
git remote add hackling hackling:ling

# Push your current sources to the VM & check them out.
# Vagrantfile provisioner creates a special command `indir'
# accepts a directory as the first argument and runs a following command in it.
git push -u hackling master && ssh hackling indir ling git checkout -f

# Build ling
ssh hackling indir ling make

# Build railing image
ssh hackling indir ling/railing ./railing image

# Run ling
ssh hackling -t indir ling/railing sudo xl create -c domain_config
```

## Running tests with Vagrant

```console
% ssh hackling -t indir ling/test make test.img
% ssh hackling -t indir ling/test sudo xl create -c domain_config

Parsing config from domain_config
Erlang [ling-0.3.2]

Eshell V6.3  (abort with ^G)
1> test:run(lists).
...
Total: 68, Ok: 68, Failed: 0
ok
2> q().
ok
3> 
Bye
Shared connection to 127.0.0.1 closed.
```
