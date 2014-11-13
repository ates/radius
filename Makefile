REBAR = $(shell which rebar 2>/dev/null || echo $(PWD)/rebar)

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

test: compile
	@$(REBAR) eunit xref
