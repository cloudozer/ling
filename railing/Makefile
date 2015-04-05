.PHONY: clean

include ../Config.mk

railing: $(patsubst %.erl,%.beam,$(wildcard *.erl)) escriptize
	./escriptize $(ARCH)

clean:
	rm -f *.beam railing

%.beam: %.erl
	erlc -DLING_VER=\"$(LING_VER)\" -DOTP_VER=\"$(OTP_VER)\" $<
