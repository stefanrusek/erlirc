VSN          := 0.1
ERL          ?= erl
EBIN_DIRS    := $(wildcard lib/*/ebin)
APP          := erlirc

all: compile

compile:
	rebar compile

docs:
	rebar skip_deps=true doc

clean: 
	@echo "removing:"
	rebar clean

build-plt:
	dialyzer --build_plt -r deps -r src --output_plt erlirc_dialyzer.plt \
		--apps kernel crypto stdlib sasl inets

dialyze: dialyze-erlirc

dialyze-erlirc:
	dialyzer --src -r src --plt erlirc_dialyzer.plt \
	-Werror_handling -Wrace_conditions -Wbehaviours

