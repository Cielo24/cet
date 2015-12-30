PROJECT := cet

REBAR = rebar3

.PHONY: all clean compile deps dev devclean devrel devshell devstart dialyzer distclean doc prod prodclean prodrel prodshell prodstart rel shell start test xref

all: compile

clean:
	@$(REBAR) clean

compile:
	@$(REBAR) compile

deps:
	@$(REBAR) upgrade

dialyzer: compile
	@$(REBAR) dialyzer

distclean:
	@rm -fR _build

doc:
	@$(REBAR) doc

shell: rel
	@(REBAR) shell

test:
	@$(REBAR) ct

xref:
	@$(REBAR) xref
