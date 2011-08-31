VSN          := 0.1
ERL          ?= erl
EBIN_DIRS    := $(wildcard lib/*/ebin)
APP          := erlirc
.PHONY: all compile deps docs clean build-plt dialyze dialyze-erlirc

all: compile

compile:
	rebar compile

deps:
	rebar get-deps

docs:
	rebar skip_deps=true doc

eunit:
	rebar skip_deps=true eunit

distclean: clean
	@echo "removing deps:"
	rm -fr deps/*

clean: 
	@echo "removing:"
	rebar clean

xref: compile
	rebar skip_deps=true xref

build-plt:
	dialyzer --build_plt -r deps -r src --output_plt erlirc_dialyzer.plt \
		--apps kernel crypto stdlib sasl inets

dialyze: dialyze-erlirc

dialyze-erlirc:
	dialyzer --src -r src --plt erlirc_dialyzer.plt \
	-Werror_handling -Wrace_conditions -Wbehaviours

