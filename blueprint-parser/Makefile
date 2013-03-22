PROJECT = katt_blueprint_parser
DIALYZER = dialyzer
REBAR = ./rebar

.PHONY:	all app deps clean clean-docs docs test eunit xref build-plt dialyze


all: app docs test

app: deps
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean: clean-docs
	$(REBAR) clean
	$(RM) ebin/*.d

clean-docs:
	$(RM) doc/*.html
	$(RM) doc/*.css
	$(RM) doc/*.png
	$(RM) doc/edoc-info

docs:
	@$(REBAR) doc skip_deps=true

test: xref eunit

xref:
	@$(REBAR) xref skip_deps=true

eunit:
	@$(REBAR) eunit skip_deps=true

build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps kernel stdlib sasl

dialyze:
	@$(DIALYZER) --src src --plt .$(PROJECT).plt --no_native \
		-Werror_handling -Wrace_conditions
