.PHONY: all mzbench get-deps compile clean test-unit test-ct check distclean

REBAR := $(shell which ./rebar || which rebar)

all: get-deps compile

run: mzbench
	./rel/mzbench/bin/mzbench console

mzbench: compile
	$(REBAR) generate

compile:
	$(REBAR) compile

get-deps:
	$(REBAR) get-deps

test-unit: all
	$(REBAR) eunit skip_deps=true

test-ct: all
	$(REBAR) ct skip_deps=true

check: test-unit test-ct

clean:
	$(REBAR) clean

distclean: clean
	rm -rf ./deps
