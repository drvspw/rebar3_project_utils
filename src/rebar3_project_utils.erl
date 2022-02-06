-module(rebar3_project_utils).

-export([init/1]).

-define(DEFAULT_PROVIDERS, [rebar3_lint_prv, rebar3_check_deps_prv]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    %% get list of providers to initalize
    Providers1 = add_release_providers(rebar_state:get(State, relx, undefined), ?DEFAULT_PROVIDERS),
    Providers2 = add_bump_provider(rebar_state:get(State, escript_name, undefined), Providers1),

    %% initialize providers
    FinalState = init_providers(State, Providers2),

    %% return
    {ok, FinalState}.

%%======================================================================
%% Private functions
%%======================================================================
init_providers(State, Providers) ->
    lists:foldl( fun(Provider, AccState) ->
                         {ok, NewState} = Provider:init(AccState),
                         NewState
                 end, State, Providers).

add_release_providers(undefined, Providers) ->
    %% not a release
    Providers;
add_release_providers(_, Providers) ->
    %% release. add deb provider
    Providers ++ [rebar3_deb_prv].

add_bump_provider(undefined, Providers) ->
    %% not an escript. add bump provider
    Providers ++ [rebar3_bump_prv];

add_bump_provider(_, Providers) ->
    %% escript.
    Providers.
