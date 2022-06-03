.PHONY: compile clean test rel run

REBAR=./rebar3

compile:
	$(REBAR) compile
	$(REBAR) format

clean:
	git clean -dXfffffffffff

test: 
	$(REBAR) fmt --verbose --check rebar.config
	$(REBAR) fmt --verbose --check "{src,include,test}/**/*.{hrl,erl,app.src}"
	$(REBAR) xref
	$(REBAR) eunit -v
	$(REBAR) ct
	$(REBAR) dialyzer

rel: $(REBAR) release


# Pass all unknown targets straight to rebar3 (e.g. `make dialyzer`)
%:
	$(REBAR) $@
