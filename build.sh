#!/bin/bash

set -e
set -x

export DEBIAN_FRONTEND=noninteractive

erlang() {
	# Get and install Erlang
	sudo sh -c 'echo "deb http://packages.erlang-solutions.com/debian wheezy contrib" > /etc/apt/sources.list.d/erlang.list'
	wget -c http://packages.erlang-solutions.com/debian/erlang_solutions.asc
	sudo apt-key add erlang_solutions.asc
	sudo apt-get update
	sudo apt-get install -y --force-yes erlang git autoconf gcc-arm-none-eabi
}

clone() {
	# Get ling
	git clone https://github.com/thenewwazoo/ling.git || true
	cd ling && git checkout raspberry-pi
}

nettle() {
	# Build and make nettle available for including and linking
	cd core/lib/    # We do this here because the ling Makefile doesn't handle cross-compilation nicely yet.
	wget -c https://ftp.gnu.org/gnu/nettle/nettle-2.7.1.tar.gz
	tar zxf nettle-2.7.1.tar.gz
	ln -s nettle-2.7.1 nettle # Makefile and includes expect nettle dir.
	cd nettle 
	./.bootstrap
	if [ ARCH = "arm" ]; then
		# newlib doesn't support assert very well, so set NDEBUG to elide asserts and prevent undefined ref errors at link time.
		env CFLAGS='-DNDEBUG --specs=nosys.specs -mfloat-abi=hard -mfpu=vfp' ./configure --host=arm-none-eabi
		make -j4 CROSS_COMPILE=$CROSS_COMPILE libnettle.a # All we need is libnettle.a and some headers.
	else
		./configure
		make -j4 libnettle.a # All we need is libnettle.a and some headers.
	fi
	cd ../../..
}

ling() {
	make CROSS_COMPILE=arm-none-eabi-
	cd railing
	CROSS_COMPILE=arm-none-eabi- ./railing image -n kernel
	mv kernel.img kernel.elf
	arm-none-eabi-objcopy -O binary kernel.elf kernel.img
	cp kernel.img /vagrant/
}


case $1 in
nettle)
	nettle
	;;
*)
	erlang; clone; nettle; ling
	;;
esac
