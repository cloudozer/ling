railing: $(patsubst %.erl,%.beam,$(wildcard *.erl))
	./escriptize

clean:
	rm -f *.beam railing

%.beam: %.erl
	erlc -DLING_VER=\"$(LING_VER)\" $< 
