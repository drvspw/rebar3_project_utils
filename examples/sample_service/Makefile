BASEDIR = $(shell pwd)
REBAR = rebar3

APPNAME = $(shell basename $(BASEDIR))
ifneq ($(APPNAME), sample_service)
  APPNAME = sample_service
endif

RELPATH = _build/default/rel/$(APPNAME)
TESTRELPATH = _build/test/rel/$(APPNAME)
PRODRELPATH = _build/prod/rel/$(APPNAME)

APPVSN = $(shell rebar3 bump)

# dialyzer
LINTERS = check-deps lint xref eunit

REGRESSION_TEST_DIR := test/
REGRESSION_TEST_FILES := $(shell find $(REGRESSION_TEST_DIR) -name '*_SUITE.erl' -exec basename '{}' \;)
REGRESSION_TEST_SUITES := $(shell echo $(REGRESSION_TEST_FILES) | sed -e 's/.erl//g' | sed -e 's/ /,/g')

.DEFAULT_GOAL := help

.PHONY: check-deps lint debian deploy tag help

compile: ## compile
	$(REBAR) compile

eunit: ## eunit
	$(REBAR) eunit

xref: ## xref
	$(REBAR) xref

dialyzer: ## dialyzer
	$(REBAR) dialyzer

lint: ## lint
	$(REBAR) lint

check-deps: ## check dependencies
	$(REBAR) check-deps

release: $(LINTERS) ## release
	$(REBAR) release

test-release: $(LINTERS) ## test release
	$(REBAR) as test release

console: release ## console
	cd $(RELPATH) && sudo ./bin/$(APPNAME) console

test: test-release test-config ## run ct test suites
	mkdir -p /tmp/var/lib/$(APPNAME) && \
	$(REBAR) ct -v --sname sample_service-test@localhost \
	--dir $(REGRESSION_TEST_DIR) \
	--suite=$(REGRESSION_TEST_SUITES) \
	--include $(BASEDIR)/include \
	--sys_config $(TESTRELPATH)/releases/$(APPVSN)/sys.config,$(TESTRELPATH)/releases/$(APPVSN)/generated.sys.config

test-coverage-report: ## Test Coverate Report
	$(REBAR) cover --verbose

test-config: ## Generate config using cuttlefish for test release
	cd $(TESTRELPATH) && bash bin/hooks/cuttlefish_pre_start test

prod-release: test ## prod release
	$(REBAR) as prod release

clean: ## clean
	$(REBAR) clean
	rm -rf _build

debian: ## create deb
	$(REBAR) as prod debian

help: ## Display help information
	@grep -E '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
