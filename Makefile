RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))

.PHONY: all mzbench get-deps compile clean test-unit test-ct check distclean

REBAR := $(abspath $(shell which ./rebar || which rebar))
SERVICE_NAME := mzbench
DEFAULT_TARGET_DIR   := $(SERVICE_NAME)

all: get-deps compile

run: mzbench
	./rel/mzbench/bin/mzbench console $(realpath $(RUN_ARGS))

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
	- dialyzer --output_plt .mzbench.plt --build_plt --apps erts kernel stdlib eunit crypto -r deps

dialyzer: .mzbench.plt
	dialyzer --plt .mzbench.plt apps/mzbench/ebin -I apps/mzbench/src -I deps

generate: get-deps compile rel
	$(eval relvsn := $(shell bin/relvsn.erl))
	$(eval target_dir ?= $(DEFAULT_TARGET_DIR))
	cd rel && $(REBAR) generate -f target_dir=$(target_dir)
	cp rel/$(target_dir)/releases/$(relvsn)/$(SERVICE_NAME).boot rel/$(target_dir)/releases/$(relvsn)/start.boot #workaround for rebar bug
	echo $(relvsn) > rel/$(target_dir)/relvsn

target: clean generate
