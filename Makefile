.NOTPARALLEL:

CHMOD := $(shell command -v chmod 2>/dev/null)
CURL := $(shell command -v curl 2>/dev/null)
DIFF := $(shell command -v diff 2>/dev/null)

OTP_RELEASE = $(shell erl -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().'  -noshell)

REBAR3 := ./rebar3.OTP$(OTP_RELEASE)
GIT_DESCRIBE := $(shell git describe --tags --first-parent --always --dirty)
SRCS := $(wildcard src/* include/* rebar.config)
SRCS := $(filter-out src/katt_blueprint.erl,$(SRCS))

ifdef CI
export KATT_DEV_MODE=true
endif

ifneq (,$(wildcard _build/DEV_MODE))
export KATT_DEV_MODE=true
endif

ifneq (,$(wildcard _build/BARE_MODE))
export KATT_BARE_MODE=true
endif

# back compat. REMOVE LATEST 2022-01-01
ifneq (,$(wildcard .rebar/DEV_MODE))
export KATT_DEV_MODE=true
endif

# back compat. REMOVE LATEST 2022-01-01
ifneq (,$(wildcard .rebar/BARE_MODE))
export KATT_BARE_MODE=true
endif

.PHONY: all
ifdef KATT_BARE_MODE
all: ebin/katt.app
else
all: ebin/katt.app bin/katt
endif

# Clean

.PHONY: conf_clean
conf_clean:
	:

.PHONY: clean
clean: $(REBAR3)
	$(REBAR3) clean

.PHONY: distclean
distclean: clean
	$(RM) -r _build
	$(RM) doc/*.html
	$(RM) doc/edoc-info
	$(RM) doc/erlang.png
	$(RM) doc/stylesheet.css
	$(RM) -r logs
	$(RM) src/katt_blueprint.erl

.PHONY: clean-tests
clean-tests:
	@ rm -rf _build/test/lib

# Docs

.PHONY: docs
docs: $(REBAR3)
	$(REBAR3) edoc

# Compile

bin/katt: escript
	mkdir -p bin
	cp -a _build/default/bin/katt bin/katt

ebin/katt.app: compile

.PHONY: escript
escript: $(REBAR3) ebin/katt.app
	$(REBAR3) escriptize
	./_build/default/bin/katt --help

.PHONY: compile
compile: $(REBAR3) $(SRCS)
	$(REBAR3) compile

# Tests

_build/DEV_MODE:
	mkdir -p _build
	touch _build/DEV_MODE

_build/BARE_MODE:
	mkdir -p _build
	touch _build/BARE_MODE

.PHONY: test
# Would be nice to include elvis to test, but it fails on OTP-18
# test: elvis
test: test_cli
test: eunit ct xref dialyzer cover

.PHONY: elvis
elvis: $(REBAR3)
	$(REBAR3) lint

.PHONY: check
check: elvis

ifdef KATT_BARE_MODE
.PHONY: test_cli
test_cli:
	: Skipping $@ in BARE_MODE
else
test_cli: _build/DEV_MODE
	./_build/default/bin/katt hostname=httpbin.org my_name=Joe your_name=Mike protocol=https: -- \
		./doc/example-httpbin.apib >test/cli || { cat test/cli && exit 1; }
	./_build/default/bin/katt from-har --apib -- ./doc/example-teapot.har > test/example-teapot.apib && \
		$(DIFF) -U0 doc/example-teapot.apib test/example-teapot.apib
endif

.PHONY: eunit
eunit: $(REBAR3) clean-tests
	$(REBAR3) eunit

.PHONY: ct
ct: $(REBAR3) clean-tests
	TEST_DIR=_build/default/test/lib/katt/test $(REBAR3) ct

.PHONY: xref
xref: $(REBAR3)
	$(REBAR3) xref

.PHONY: dialyzer
dialyzer: $(REBAR3)
	$(REBAR3) dialyzer

.PHONY: cover
cover: $(REBAR3) clean-tests
	$(REBAR3) cover -v

.PHONY: publish
publish: $(REBAR3) docs
	$(REBAR3) hex publish -r hexpm --yes

# https://github.com/erlang/rebar3/issues/2903
./rebar3.OTP18:
	$(CURL) -qfsSL -o $@ https://github.com/erlang/rebar3/releases/download/3.15.3/rebar3 && $(CHMOD) +x $@
./rebar3.OTP19:
	$(CURL) -qfsSL -o $@ https://github.com/erlang/rebar3/releases/download/3.15.2/rebar3 && $(CHMOD) +x $@
./rebar3.OTP20:
	$(CURL) -qfsSL -o $@ https://github.com/erlang/rebar3/releases/download/3.15.2/rebar3 && $(CHMOD) +x $@
./rebar3.OTP21:
	$(CURL) -qfsSL -o $@ https://github.com/erlang/rebar3/releases/download/3.15.2/rebar3 && $(CHMOD) +x $@
./rebar3.OTP22:
	$(CURL) -qfsSL -o $@ https://github.com/erlang/rebar3/releases/download/3.18.0/rebar3 && $(CHMOD) +x $@
./rebar3.OTP23:
	$(CURL) -qfsSL -o $@ https://github.com/erlang/rebar3/releases/download/3.19.0/rebar3 && $(CHMOD) +x $@
./rebar3.OTP24:
	$(CURL) -qfsSL -o $@ https://github.com/erlang/rebar3/releases/download/3.20.0/rebar3 && $(CHMOD) +x $@
./rebar3.OTP25:
	$(CURL) -qfsSL -o $@ https://github.com/erlang/rebar3/releases/download/3.21.0/rebar3 && $(CHMOD) +x $@
./rebar3.OTP26:
	$(CURL) -qfsSL -o $@ https://github.com/erlang/rebar3/releases/download/3.23.0/rebar3 && $(CHMOD) +x $@
./rebar3.OTP27:
	$(CURL) -qfsSL -o $@ https://github.com/erlang/rebar3/releases/download/3.23.0/rebar3 && $(CHMOD) +x $@
./rebar3.OTP28:
	$(CURL) -qfsSL -o $@ https://github.com/erlang/rebar3/releases/download/3.25.0/rebar3 && $(CHMOD) +x $@

.PHONY: docker
docker:
	if git tag | grep -q -Fx "$(GIT_DESCRIBE)"; then \
		$(MAKE) docker-force; \
	else \
		echo "Current version $(GIT_DESCRIBE) isn't in 'git tag'."; \
		echo "Run 'make docker-force' if you really want to build and push a $(GIT_DESCRIBE) version."; \
		exit 1; \
	fi

.PHONY: docker-force
docker-force:
	# docker context create aws-docker-amd64 --docker host=ssh://ec2-13-51-198-153.eu-north-1.compute.amazonaws.com
	# docker context create aws-docker-arm64 --docker host=ssh://ec2-13-48-46-86.eu-north-1.compute.amazonaws.com
	# docker buildx create --name aws-multiarch-builder aws-docker-amd64
	# docker buildx create --name aws-multiarch-builder --append aws-docker-arm64
	# docker buildx use aws-multiarch-builder
	docker buildx build . \
		--push \
    --platform linux/amd64,linux/arm64 \
		--tag ysoftwareab/katt:$(GIT_DESCRIBE) \
		--tag ysoftwareab/katt:latest \
		--build-arg FROM=erlang:slim \
		--build-arg LABEL_VCS_REF=$$(git rev-parse HEAD) \
		--build-arg LABEL_BUILD_DATE=$$(date -u +"%Y-%m-%dT%H:%M:%SZ")
