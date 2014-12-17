test.img: $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))
	../railing/railing image -ltest_server -lcompiler -lcrypto -lsyntax_tools -lsasl -los_mon -ipriv -m 1024

clean:
	rm -rf .railing test.img test.dom ebin/*.beam

ebin/%.beam: src/%.erl
	erlc -o ebin $<
