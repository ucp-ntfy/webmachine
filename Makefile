ERL          ?= erl
APP          := webmachine

REPO = ${shell echo `basename "$${PWD}"`}
ARTIFACTSFILE = ${shell echo ${REPO}-`date +%F_%H-%M-%S`.tgz}

.PHONY: deps

all: deps compile

compile: deps
	./rebar3 compile

deps: DEV_MODE
	@(./rebar3 get-deps)

clean:
	@(./rebar3 clean)

# nuke deps first to avoid wasting time having rebar3 recurse into deps
# for clean
distclean:
	@rm -rf ./deps ./.rebar3
	@(./rebar3 clean)

edoc:
	@$(ERL) -noshell -run edoc_run application '$(APP)' '"."' '[{preprocess, true},{includes, ["."]}]'
DIALYZER_APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = $(HOME)/.webmachine_dialyzer_plt

verbosetest: all
	@(./rebar3 -v skip_deps=true eunit)

travisupload:
	tar cvfz ${ARTIFACTSFILE} --exclude '*.beam' --exclude '*.erl' test.log .eunit
	travis-artifacts upload --path ${ARTIFACTSFILE}

DEV_MODE:
	@[ -d ./.rebar3 ] || mkdir ./.rebar3
	@touch ./.rebar3/DEV_MODE
