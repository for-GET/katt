.NOTPARALLEL:

.PHONY:	all conf_clean compile get-deps update-deps delete-deps doc xref test eunit clean

all: get-deps compile xref

compile:
	./rebar compile

get-deps:
	./rebar get-deps

update-deps:
	./rebar update-deps

delete-deps:
	./rebar delete-deps

docs:
	./rebar doc skip_deps=true

xref:
	./rebar xref skip_deps=true

test: eunit dialyzer

dialyzer:
	dialyzer -Ideps --src $(shell find src -name *.erl -not -name katt_blueprint.erl)

eunit:
	./rebar eunit skip_deps=true

conf_clean:
	@:

clean:
	./rebar clean
	$(RM) doc/*.html
	$(RM) doc/*.png
	$(RM) doc/*.css
	$(RM) doc/edoc-info
	$(RM) ebin/*.d
	$(RM) src/katt_blueprint.erl
