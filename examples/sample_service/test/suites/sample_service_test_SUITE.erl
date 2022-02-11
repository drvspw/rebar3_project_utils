-module(sample_service_test_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl"). % Eunit macros for convenience

-export([all/0
        ,groups/0
        ,init_per_suite/1, end_per_suite/1
        ,init_per_group/2, end_per_group/2
         %%,init_per_testcase/2, end_per_testcase/2
        ]).

-export([
         sample_service_test/1
        ]).

%%=================================================
%% Test Setup
%%=================================================
all() ->
  [{group, sample_service_tests}].

groups() ->
  [{sample_service_tests, [], [sample_service_test]}].

init_per_suite(Config) ->
  {ok, _} = application:ensure_all_started(sample_service),
  Config.

end_per_suite(_Config) ->
  application:stop(sample_service),
  ok.

init_per_group(sample_service_tests, Config) ->
  %% test setup for group sample_service_tests
  Config;

init_per_group(_, Config) ->
  Config.

end_per_group(sample_service_tests, _Config) ->
  %% teardown test setup for group sample_service_tests
  ok;

end_per_group(_Name, _Config) ->
  ok.


%%=================================================
%% Test Cases
%%=================================================
sample_service_test(_Config) ->
  {ok, PoolSize} =  application:get_env(sample_service, pool_size),
  ?assertEqual(1, PoolSize),
  ?assert(true).
