REBAR=rebar3

all: compile

compile:
	$(REBAR) compile
	$(REBAR) escriptize

check_all: test lint

test: xref dialyzer eunit

dialyzer:
	$(REBAR) dialyzer

xref:
	$(REBAR) xref

eunit:
	$(REBAR) eunit

lint:
	$(REBAR) as lint lint

clean:
	$(REBAR) clean -a

.PHONY: all compile check_all test dialyzer xref eunit lint clean
