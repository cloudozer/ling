.PHONY: clean

include ../Config.mk

railing: $(patsubst %.erl,%.beam,$(wildcard *.erl)) escriptize
	./escriptize $(ARCH)

clean:
	rm -f *.beam railing

%.beam: %.erl
	erlc -DLING_VER=\"$(LING_VER)\" -DARCH=\'$(ARCH)\' -DOTP_VER=\"$(OTP_VER)\" $<
