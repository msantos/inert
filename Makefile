REBAR=$(shell which rebar || echo ./rebar)

all: compile

./rebar:
	erl -noshell -s inets start -s ssl start \
		-eval 'httpc:request(get, {"https://raw.github.com/wiki/rebar/rebar/rebar", []}, [], [{stream, "./rebar"}])' \
		-s inets stop -s init stop
	chmod +x ./rebar

compile: $(REBAR)
	@$(REBAR) compile

clean: $(REBAR)
	@$(REBAR) clean

deps: $(REBAR)
	@$(REBAR) get-deps

test: compile
	@$(REBAR) xref eunit recursive=false

examples: eg
eg:
	@erlc -I deps -o ebin examples/*.erl

.PHONY: test
