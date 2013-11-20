ERL ?= erl
APP := vortex_indexer

.PHONY: deps test

all: deps
	@./rebar compile

deps:
	@./rebar get-deps

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

docs:
	@erl -noshell -run edoc_run application '$(APP)' '"."' '[]'

test:
	rm -rf .eunit
	@./rebar -C test.config skip_deps=true eunit

