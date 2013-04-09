suite=$(if $(SUITE), suite=$(SUITE), )
# Guess whether we are part of a kred build, we don't want rebar
ifndef BUILD
ifdef OUR_SYSTEM_APPS
  BUILD = kred
  SKIP_DEPS = "skip_deps=true"
else
  BUILD = normal
  SKIP_DEPS =
endif
endif

export BUILD
export SKIP_DEPS
REBAR = $(realpath ./rebar)


.PHONY:	all compile get-deps update-deps delete-deps doc xref test eunit clean

all: compile xref

compile: get-deps
	$(REBAR) compile $(SKIP_DEPS)

get-deps:
ifneq ($(BUILD), kred)
	$(REBAR) get-deps
endif

update-deps:
ifneq ($(BUILD), kred)
	$(REBAR) update-deps
endif

delete-deps:
ifneq ($(BUILD), kred)
	$(REBAR) delete-deps
endif

docs:
	./rebar doc skip_deps=true

xref:
	./rebar xref skip_deps=true

test: eunit

eunit:
	./rebar eunit skip_deps=true

conf_clean:
	@:

clean:
	$(REBAR) clean $(SKIP_DEPS)
	$(RM) doc/*.html
	$(RM) doc/*.png
	$(RM) doc/*.css
	$(RM) doc/edoc-info
	$(RM) ebin/*.d
