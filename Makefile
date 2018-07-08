.NOTPARALLEL:

REBAR ?= $(shell command -v rebar >/dev/null 2>&1 && echo "rebar" || echo "$(CURDIR)/rebar")

ELVIS ?= $(shell command -v elvis >/dev/null 2>&1 && echo "elvis" || echo "$(CURDIR)/elvis")

DEPS_PLT := $(CURDIR)/.deps_plt

ERLANG_DIALYZER_APPS := erts \
						kernel \
						stdlib \
						crypto \
						asn1 \
						public_key \
						ssl

DIALYZER := dialyzer
ifdef CI
export KATT_DEV_MODE=true
endif

ifneq (,$(wildcard .rebar/DEV_MODE))
export KATT_DEV_MODE=true
endif

ifneq (,$(wildcard .rebar/BARE_MODE))
export KATT_BARE_MODE=true
endif

# Travis CI is slow at building dialyzer PLT
TRAVIS ?=
ifeq (true,$(TRAVIS))
	OTP_VSN := $(shell erl -noshell -eval 'io:format("~p", [erlang:system_info(otp_release)]), erlang:halt(0).' | perl -lne 'print for /^(?:"R)?(\d+).*/g')
	NO_DIALYZER := $(shell expr $(OTP_VSN) \<= 14 )

	ifeq ($(NO_DIALYZER), 1)
		DIALYZER := : not running dialyzer on TRAVIS with R14 and below
	endif
endif

SRCS := $(wildcard src/* include/* rebar.config)

SRC_BEAMS := $(patsubst src/%.erl, ebin/%.beam, $(wildcard src/*.erl))

.PHONY: all
all: deps ebin/katt.app bin/katt

# Clean

.PHONY: conf_clean
conf_clean:
	:

.PHONY: clean
clean:
	$(REBAR) clean
	$(RM) -r .rebar
	$(RM) -r bin
	$(RM) doc/*.html
	$(RM) doc/edoc-info
	$(RM) doc/erlang.png
	$(RM) doc/stylesheet.css
	$(RM) -r ebin
	$(RM) -r logs
	$(RM) src/katt_blueprint.erl

.PHONY: distclean
distclean:
	$(RM) $(DEPS_PLT)
	$(RM) -r deps
	$(MAKE) clean

# Deps

.PHONY: get-deps
get-deps:
	$(REBAR) get-deps

.PHONY: update-deps
update-deps:
	$(REBAR) update-deps

.PHONY: delete-deps
delete-deps:
	$(REBAR) delete-deps

.PHONY: deps
deps: get-deps

# Docs

.PHONY: docs
docs:
	$(REBAR) doc skip_deps=true

# Compile

ebin/katt.app: compile

.PHONY: bin/katt
ifdef KATT_BARE_MODE
bin/katt:
	: Skipping $@ in BARE_MODE
else
bin/katt: ebin/katt.app $(SRC_BEAMS)
	$(REBAR) escriptize
	bin/katt --help
endif

.PHONY: compile
compile: $(SRCS)
	$(REBAR) compile

# Tests

.rebar/DEV_MODE:
	mkdir -p .rebar
	touch .rebar/DEV_MODE

.rebar/BARE_MODE:
	mkdir -p .rebar
	touch .rebar/BARE_MODE

.PHONY: xref
xref:
	$(REBAR) xref skip_deps=true

.PHONY: test
test: .rebar/DEV_MODE deps test_cli eunit xref dialyzer

ifdef KATT_BARE_MODE
.PHONY: test_cli
test_cli:
	: Skipping $@ in BARE_MODE
else
test_cli: .rebar/DEV_MODE deps
	bin/katt hostname=httpbin.org my_name=Joe your_name=Mike protocol=https: -- ./doc/example-httpbin.apib >test/cli 2>/dev/null || { cat test/cli && exit 1; }
	bin/katt from-har --apib -- ./doc/example-teapot.har > test/example-teapot.apib && diff -U0 doc/example-teapot.apib test/example-teapot.apib
endif

.PHONY: eunit
eunit:
	$(REBAR) eunit skip_deps=true

$(DEPS_PLT):
	$(DIALYZER) --build_plt --apps $(ERLANG_DIALYZER_APPS) -r deps --output_plt $(DEPS_PLT)

.PHONY: dialyzer
dialyzer: $(DEPS_PLT)
	$(DIALYZER) -q --plt $(DEPS_PLT) --src $(shell find src -name *.erl -not -name katt_blueprint.erl) > test/dialyzer_warnings || true
	diff -U0 test/known_dialyzer_warnings test/dialyzer_warnings

.PHONY: elvis
elvis:
	$(ELVIS) rock > test/elvis || true
	grep "FAIL" test/elvis | sed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g" > test/elvis_warnings
	diff -U0 test/known_elvis_warnings test/elvis_warnings || cat test/elvis
