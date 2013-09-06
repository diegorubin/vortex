ERL ?= erl
APP := vortex

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin

REBAR="./rebar"
ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

.PHONY: test deps

all:
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
	@./rebar -C test.config skip_deps=true eunit

shell:
	@$(ERL) $(ERLFLAGS)

