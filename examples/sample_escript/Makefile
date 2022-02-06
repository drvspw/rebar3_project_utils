BASEDIR = $(shell pwd)
REBAR = rebar3

APPNAME = $(shell basename $(BASEDIR))
ifneq ($(APPNAME), sample_escript)
  APPNAME = sample_escript
endif

LINTERS = check-deps lint xref eunit
RELPATH = _build/default/bin

.PHONY: run

.DEFAULT_GOAL := help

compile: ## compile
	$(REBAR) compile

eunit: ## run eunit tests
	$(REBAR) eunit

xref: ## xref analysis
	$(REBAR) xref

dialyzer: ## dialyzer
	$(REBAR) dialyzer

check-deps: ## check dependencies
	$(REBAR) check-deps

lint: ## lint
	$(REBAR) lint

release: $(LINTERS) ## create escript
	$(REBAR) escriptize

run: release ## run escript
	$(RELPATH)/$(APPNAME) $(filter-out $@,$(MAKECMDGOALS))
%:
	@:

clean: ## clean
	$(REBAR) clean
	rm -rf _build

test-coverage-report: ## generate test coverage report
	$(REBAR) cover --verbose

help: ## Display help information
	@grep -E '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
