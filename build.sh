#!/bin/bash

set -e
set -x

export DEBIAN_FRONTEND=noninteractive

# Get and install Erlang
echo "deb http://packages.erlang-solutions.com/debian wheezy contrib" > /etc/apt/sources.list.d/erlang.list
wget -c http://packages.erlang-solutions.com/debian/erlang_solutions.asc
sudo apt-key add erlang_solutions.asc
apt-get update
apt-get install -y --force-yes erlang git autoconf

# Get and prepare cross-compilation tools
git clone https://github.com/raspberrypi/tools || true
export PATH=$PATH:$(pwd)/tools/arm-bcm2708/gcc-linaro-arm-linux-gnueabihf-raspbian-x64/bin

# Get ling
git clone https://github.com/thenewwazoo/ling.git || true
cd ling && git checkout raspberry-pi

# Build and make nettle available for including and linking

cd core/lib/    # We do this here because the ling Makefile doesn't handle cross-compilation nicely yet.
wget -c https://ftp.gnu.org/gnu/nettle/nettle-2.7.1.tar.gz
tar zxf nettle-2.7.1.tar.gz
mv nettle-2.7.1 nettle # rename because ling makefile expects it
cd nettle 
./.bootstrap
./configure --host=arm-linux-gnueabihf
make CROSS_COMPILE=arm-linux-gnueabihf-
cd ../../..

make CROSS_COMPILE=arm-linux-gnueabihf-
echo "Railing built... what next? :)"
