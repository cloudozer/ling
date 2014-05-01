
default: otp nettle uclibc xen

fetch: otp.tar.gz nettle.tar.gz uclibc.tar.bz2 xen.tar.gz

otp.tar.gz:
	wget -O $@ -c http://www.erlang.org/download/otp_src_R16B01.tar.gz

nettle.tar.gz:
	wget -O $@ -c http://www.lysator.liu.se/~nisse/archive/nettle-2.7.1.tar.gz

uclibc.tar.bz2:
	wget -O $@ -c http://www.uclibc.org/downloads/uClibc-0.9.33.2.tar.bz2

xen.tar.gz:
	wget -O $@ -c http://www.xenproject.org/downloads/xen-archives/xen-44-series/xen-440/299-xen-project-440/file.html

OTP= otp_src_R16B01

$(OTP): otp.tar.gz
	tar xvzf $<

otp: $(OTP)
	(cd $< && env MAKEFLAGS=-j ./otp_build setup --disable-hipe --without-javac && make -j install)

NETTLE= nettle-2.7.1

$(NETTLE): nettle.tar.gz
	tar xvzf $<

nettle: $(NETTLE)
	(cd $< && ./configure --disable-public-key --disable-shared --disable-pic --disable-openssl CFLAGS="-fno-stack-protector -U_FORTIFY_SOURCE" && make -j install)


UCLIBC=	uClibc-0.9.33.2

$(UCLIBC): uclibc.tar.bz2
	tar xvjf $<

uclibc: $(UCLIBC) uclibc-config
	(cd $< && env C_INCLUDE_PATH=/usr/include/$(shell arch)-linux-gnu make -j install)

uclibc-config:
	install $(CURDIR)/uClibc_config $(UCLIBC)/.config

XEN= xen-4.4.0

xen: $(XEN)

$(XEN): xen.tar.gz
	tar xvzf $<

cleanall:
	rm -rf $(UCLIBC) $(OTP) $(NETTLE) # do not remove xen

.PHONY: cleanall default fetch otp nettle uclibc xen
