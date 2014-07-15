RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))

.PHONY: all mz_bench get-deps compile clean test-unit test-ct check distclean run

REBAR := $(abspath $(shell which ./rebar || which rebar))
SERVICE_PREFIX=/mz
RPM_NAME=mz-bench
SERVICE_NAME=mz_bench
SERVICE_LOG_DIR=/var/log/${SERVICE_NAME}
DEFAULT_TARGET_DIR=${SERVICE_NAME}

all: get-deps compile

run: mz_bench
	./rel/mz_bench/bin/mz_bench console $(realpath $(RUN_ARGS))

mz_bench: compile
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
	rm -rf ./deps .mz_bench.plt

.mz_bench.plt:
	- dialyzer --output_plt .mz_bench.plt --build_plt --apps erts kernel stdlib eunit crypto -r deps

dialyzer: .mz_bench.plt
	- dialyzer --plt .mz_bench.plt apps/mz_bench/ebin -I apps/mz_bench/src -I deps -o dialyzer.log \
		-Wunmatched_returns \
		-Werror_handling \
		-Wrace_conditions \
		-Wunderspecs

generate: get-deps compile rel
	$(eval relvsn := $(shell bin/relvsn.erl))
	$(eval target_dir ?= $(DEFAULT_TARGET_DIR))
	cd rel && $(REBAR) generate -f target_dir=$(target_dir)
	cp rel/$(target_dir)/releases/$(relvsn)/$(SERVICE_NAME).boot rel/$(target_dir)/releases/$(relvsn)/start.boot #workaround for rebar bug
	echo $(relvsn) > rel/$(target_dir)/relvsn

# RPM creation
change-version:
	@if test "$(new-version)" = "" ; then \
		echo "new-version is undefined, use 'make change-version new-version=X.X.X' command"; \
		exit 1; \
	fi
	$(eval relvsn := $(shell bin/relvsn.erl))
	sed -i "s/${relvsn}/$(new-version)/g" rel/reltool.config
	git commit rel/reltool.config -m "Bump mz-bench version to $(new-version)"

##
## RPM
##

PKG_NAME     = ${SERVICE_NAME}
PKG_VERSION := $(shell bin/relvsn.erl)
PKG_PREFIX   = ${SERVICE_PREFIX}
PKG_DEPENDS  = erlang>=15.03

PKG_AFTER_INSTALL = package-scripts/POSTIN
PKG_AFTER_REMOVE  = package-scripts/POSTUN

FPMOPTS      = --template-scripts \
               --template-value="prefix=${SERVICE_PREFIX}"

-include mz-rpm.mk

.PHONY: pkgroot
pkgroot: generate
	rm -rf pkgroot
	mkdir  pkgroot
	cp -rt pkgroot -- rel/*

