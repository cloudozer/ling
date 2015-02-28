#!/bin/bash

set -e
set -x

export DEBIAN_FRONTEND=noninteractive

# Get and install Erlang
sudo sh -c 'echo "deb http://packages.erlang-solutions.com/debian wheezy contrib" > /etc/apt/sources.list.d/erlang.list'
wget -c http://packages.erlang-solutions.com/debian/erlang_solutions.asc
sudo apt-key add erlang_solutions.asc
sudo apt-get update
sudo apt-get install -y --force-yes erlang git autoconf gcc-arm-none-eabi

# Get ling
git clone https://github.com/thenewwazoo/ling.git || true
cd ling && git checkout raspberry-pi

# Build and make nettle available for including and linking
cd core/lib/    # We do this here because the ling Makefile doesn't handle cross-compilation nicely yet.
wget -c https://ftp.gnu.org/gnu/nettle/nettle-2.7.1.tar.gz
tar zxf nettle-2.7.1.tar.gz
ln -s nettle-2.7.1 nettle # Makefile and includes expect nettle dir.
cd nettle 
./.bootstrap
# newlib doesn't support assert very well, so set NDEBUG to elide asserts and prevent undefined ref errors at link time.
CFLAGS='-DNDEBUG --specs=nosys.specs -mfloat-abi=hard -mfpu=vfp' ./configure --host=arm-none-eabi
make -j4 CROSS_COMPILE=arm-none-eabi- libnettle.a # All we need is libnettle.a and some headers.
cd ../../..

make CROSS_COMPILE=arm-none-eabi-
cd railing
CROSS_COMPILE=arm-none-eabi- ./railing image -n kernel
mv kernel.img kernel.elf
arm-none-eabi-objcopy -O binary kernel.elf kernel.img
cp kernel.img /vagrant/
