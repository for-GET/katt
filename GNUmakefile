.NOTPARALLEL:

# Travis CI is slow at building dialyzer PLT
OTP_VSN=$(shell erl -noshell -eval 'io:format("~p", [erlang:system_info(otp_release)]), erlang:halt(0).' | perl -lne 'print for /R(\d+).*/g')
TRAVIS_SLOW=$(shell expr $(OTP_VSN) \<= 14 )

ifeq ($(TRAVIS_SLOW), 0)
	DIALYZER=$(shell which dialyzer)
else
	DIALYZER=: not running dialyzer on R14
endif

DEPS_PLT = $(CURDIR)/.deps_plt

ERLANG_DIALYZER_APPS = erts \
				       kernel \
                       ssl \
                       stdlib


.PHONY:	all conf_clean compile get-deps update-deps delete-deps doc xref test eunit clean dialyzer distclean

all: get-deps compile xref

compile:
	@./rebar compile

get-deps:
	@./rebar get-deps

update-deps:
	@./rebar update-deps

delete-deps:
	@./rebar delete-deps

docs:
	@./rebar doc skip_deps=true

xref:
	@./rebar xref skip_deps=true

test: eunit dialyzer

eunit:
	@./rebar eunit skip_deps=true

conf_clean:
	@:

clean:
	@./rebar clean
	@$(RM) doc/*.html
	@$(RM) doc/*.png
	@$(RM) doc/*.css
	@$(RM) doc/edoc-info
	@$(RM) ebin/*.d
	@$(RM) src/katt_blueprint.erl

$(DEPS_PLT):
	@$(DIALYZER) --build_plt --apps $(ERLANG_DIALYZER_APPS) -r deps --output_plt $(DEPS_PLT)

dialyzer: $(DEPS_PLT)
	@$(DIALYZER) --plt $(DEPS_PLT) --src $(shell find src -name *.erl -not -name katt_blueprint.erl)

distclean:
	@rm -rf deps $(DEPS_PLT)
	@$(MAKE) clean
