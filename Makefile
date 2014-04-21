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

check: test-unit test-ct dialyzer

clean:
	$(REBAR) clean

distclean: clean
	rm -rf ./deps .mzbench.plt

.mzbench.plt:
	        - dialyzer --output_plt .mzbench.plt --build_plt --apps erts kernel stdlib eunit crypto

dialyzer: .mzbench.plt
	        dialyzer --plt .mzbench.plt --src apps/mzbench/src -I apps/mzbench/src
