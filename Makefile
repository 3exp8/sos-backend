.PHONY: all rel deps compile clean

all: deps compile

compile: rebar3
	#./rebar3 compile ||  (cd _build/default/lib/fcm && make && cd ../../../../ && pwd && ./rebar3 compile)
	./rebar3 compile
rel : rebar3
	./rebar3 release

test : rebar3
	./rebar3 eunit
clean:
	rm -rf _build/default/lib/crossbar/ebin/*
	rm _build/default/lib/db/ebin/*

cleanall:
