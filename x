make -B -C core/lib/lwip
rm core/ling_main.*
rm core/ol_tcp.o
make
make -B -C railing
cp railing/railing ~/testling

