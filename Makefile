REBAR = ./rebar

all: compile

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	rm -f *.profile

test: compile
	@$(REBAR) eunit xref
