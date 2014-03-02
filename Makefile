.PHONY: test

ERL=erl
BEAMDIR=./deps/*/ebin ./ebin
REBAR=./rebar
DIALYZER=dialyzer

all: update-deps get-deps clean dev #test generate

dev: compile xref edoc


generate:
	cd rel && .$(REBAR) generate

update-deps:
	@$(REBAR) update-deps

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

xref:
	@$(REBAR) xref skip_deps=true

clean:
	@ $(REBAR) clean;

test:
	@$(REBAR) skip_deps=true eunit

edoc:
	@$(REBAR) skip_deps=true doc

dialyzer: compile
	@$(DIALYZER) -I include ebin

setup-dialyzer:
	@$(DIALYZER) --build_plt --apps kernel stdlib mnesia eunit erts crypto
