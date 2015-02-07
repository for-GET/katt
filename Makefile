export PATH := $(CURDIR):$(PATH)

PROJECT = katt
DEPS = mochijson3 lhttpc neotoma
TEST_DEPS = meck

dep_mochijson3 = git git://github.com/tophitpoker/mochijson3.git 1a1c913ac80bb45d3de5fbd74d21e96c45e9e844
dep_lhttpc = git git://github.com/waisbrot/lhttpc.git 824a89316d59353181990f5a461157751ca67907
dep_neotoma = git git://github.com/seancribbs/neotoma.git 1.6.1
dep_meck = git git://github.com/eproxus/meck.git 0.8.2

ERLC_OPTS ?= -Werror \
				+debug_info \
				+warn_unused_vars \
				+warn_shadow_vars \
				+warn_unused_import \
				+warn_export_all \
				+warn_export_vars \
				+warn_obsolete_vars \
				+warn_untyped_recored \
				+warnings_as_errors

CT_SUITES = katt
CT_OPTS = -cover test/cover.spec
PLT_APPS = ssl # erts kernel stdlib # included by default

all::

ebin/$(PROJECT).app:: src/katt_blueprint.erl

include erlang.mk

deps:: src/katt_blueprint.erl

clean::
	$(RM) src/katt_blueprint.erl

# EXTRA

src/katt_blueprint.erl:
	$(CURDIR)/priv/compile-parser

test: eunit dialyze
