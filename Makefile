.NOTPARALLEL:

REBAR ?= $(shell which rebar 2> /dev/null || which ./rebar)

DEPS_PLT := $(CURDIR)/.deps_plt

ERLANG_DIALYZER_APPS := erts \
					    kernel \
					    ssl \
					    stdlib

DIALYZER := $(shell which dialyzer)

# Travis CI is slow at building dialyzer PLT
ifeq ($(TRAVIS), true)
	OTP_VSN := $(shell erl -noshell -eval 'io:format("~p", [erlang:system_info(otp_release)]), erlang:halt(0).' | perl -lne 'print for /R(\d+).*/g')
	SLOW_DIALYZER := $(shell expr $(OTP_VSN) \<= 14 )

	ifeq ($(SLOW_DIALYZER), 1)
		DIALYZER := : not running dialyzer on TRAVIS with R14
	endif
endif


.PHONY: all compile get-deps update-deps delete-deps doc xref test eunit conf_clean clean dialyzer distclean

all: get-deps compile xref

compile:
	@$(REBAR) compile

get-deps:
	@$(REBAR) get-deps

update-deps:
	@$(REBAR) update-deps

delete-deps:
	@$(REBAR) delete-deps

docs:
	@$(REBAR) doc skip_deps=true

xref:
	@$(REBAR) xref skip_deps=true

test: eunit dialyzer

eunit:
	@$(REBAR) eunit skip_deps=true

conf_clean:
	@:

clean:
	@$(REBAR) clean
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
