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
	sudo apt-get install -y --force-yes erlang git autoconf gcc-arm-none-eabi make
}

clone() {
	git clone https://github.com/cloudozer/ling.git
	cd ling
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
ling)
	ling
	;;
*)
	erlang; clone; ling
	;;
esac
