suite=$(if $(SUITE), suite=$(SUITE), )

.PHONY:	all compile get-deps update-deps delete-deps doc xref test eunit clean

all: get-deps compile xref

compile:
	./rebar compile
	@ rm ebin/katt_blueprint.beam
	@ priv/compile-parser

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

test: eunit

eunit:
	@ erlc -I include -pa ebin -pa test -o ebin test/*.erl
	@ erl -noshell -pa ebin -eval "eunit:test(katt_blueprint_parse_tests, [])" -s init stop

conf_clean:
	@:

clean:
	./rebar clean
	$(RM) doc/*.html
	$(RM) doc/*.png
	$(RM) doc/*.css
	$(RM) doc/edoc-info
	$(RM) ebin/*.d
