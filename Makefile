.NOTPARALLEL:

REBAR ?= $(shell command -v rebar >/dev/null 2>&1 && echo "rebar" || echo "$(CURDIR)/rebar")

ELVIS ?= $(shell command -v elvis >/dev/null 2>&1 && echo "elvis" || echo "$(CURDIR)/elvis")

DEPS_PLT := $(CURDIR)/.deps_plt

ERLANG_DIALYZER_APPS := erts \
					    kernel \
					    ssl \
					    stdlib

DIALYZER := dialyzer

# Travis CI is slow at building dialyzer PLT
ifeq ($(TRAVIS), true)
	OTP_VSN := $(shell erl -noshell -eval 'io:format("~p", [erlang:system_info(otp_release)]), erlang:halt(0).' | perl -lne 'print for /R(\d+).*/g')
	SLOW_DIALYZER := $(shell expr $(OTP_VSN) \<= 14 )

	ifeq ($(SLOW_DIALYZER), 1)
		DIALYZER := : not running dialyzer on TRAVIS with R14
	endif
endif


.PHONY: all
all: deps ebin/katt.app

.PHONY: compile
compile:
	$(REBAR) compile

.PHONY: get-deps
get-deps:
	$(REBAR) get-deps

.PHONY: update-deps
update-deps:
	$(REBAR) update-deps

.PHONY: delete-deps
delete-deps:
	$(REBAR) delete-deps

.PHONY: docs
docs:
	$(REBAR) doc skip_deps=true

.PHONY: xref
xref:
	$(REBAR) xref skip_deps=true

.PHONY: elvis
elvis:
	$(ELVIS) rock

.PHONY: test
test: eunit dialyzer

.PHONY: eunit
eunit:
	$(REBAR) eunit skip_deps=true

.PHONY: conf_clean
conf_clean:
	:

.PHONY: clean
clean:
	$(REBAR) clean
	$(RM) doc/*.html
	$(RM) doc/*.png
	$(RM) doc/*.css
	$(RM) doc/edoc-info
	$(RM) ebin/*.d
	$(RM) src/katt_blueprint.erl

$(DEPS_PLT):
	$(DIALYZER) --build_plt --apps $(ERLANG_DIALYZER_APPS) -r deps --output_plt $(DEPS_PLT)

.PHONY: dialyzer
dialyzer: $(DEPS_PLT)
	$(DIALYZER) --plt $(DEPS_PLT) --src $(shell find src -name *.erl -not -name katt_blueprint.erl)

.PHONY: distclean
distclean:
	rm -rf deps $(DEPS_PLT)
	$(MAKE) clean

ebin/katt.app:
	$(MAKE) compile

deps:
	$(MAKE) get-deps
