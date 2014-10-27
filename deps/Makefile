
default: otp nettle

fetch: otp.tar.gz nettle.tar.gz

otp.tar.gz:
	wget -O $@ -c http://www.erlang.org/download/otp_src_R16B01.tar.gz

nettle.tar.gz:
	wget -O $@ -c http://www.lysator.liu.se/~nisse/archive/nettle-2.7.1.tar.gz

OTP= otp_src_R16B01

$(OTP): otp.tar.gz
	tar xvzf $<

otp: $(OTP)
	(cd $< && ./otp_build setup --disable-hipe --without-javac && make install)

NETTLE= nettle-2.7.1

$(NETTLE): nettle.tar.gz
	tar xvzf $<

nettle: $(NETTLE)
	(cd $< && ./configure --disable-public-key --disable-shared --disable-pic --disable-openssl CFLAGS="-fno-stack-protector -U_FORTIFY_SOURCE" && make -j install)

cleanall:
	rm -rf $(OTP) $(NETTLE)

.PHONY: cleanall default fetch otp nettle
