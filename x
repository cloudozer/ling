#!/usr/bin/bash
#make -B -C core/lib/lwip
#rm core/ol_tcp.o
rm core/ling_main.*
make
make -B -C railing
cp railing/railing ~/BWT

