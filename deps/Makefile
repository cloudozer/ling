
default: otp

fetch: otp.tar.gz

otp.tar.gz:
	wget -O $@ -c http://www.erlang.org/download/otp_src_R16B01.tar.gz

OTP= otp_src_R16B01

$(OTP): otp.tar.gz
	tar xvzf $<

otp: $(OTP)
	(cd $< && ./otp_build setup --disable-hipe --without-javac)
	$(MAKE) -C $< install

cleanall:
	rm -rf $(OTP)

.PHONY: cleanall default fetch otp
