PROJECT = radius

REBAR = $(shell which rebar 2>/dev/null || echo $(PWD)/rebar)
DIALYZER = dialyzer

all: compile

compile:
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

test: compile
	@$(REBAR) eunit xref

build-plt:
	@$(DIALYZER) --build_plt --output_plt $(PROJECT).plt \
        --apps erts kernel stdlib compiler crypto asn1

dialyze:
	@$(DIALYZER) --src src --plt $(PROJECT).plt \
        -Werror_handling -Wrace_conditions -Wunmatched_returns
