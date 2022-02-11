%% Copyright Notice
%%   Some of the code in this file is taken from https://github.com/vernemq/rebar3_cuttlefish repo
%%
-module(rebar3_cuttlefish_tar_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, tar).
-define(NAMESPACE, default).
-define(DEPS, [{?NAMESPACE, release}]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {namespace, ?NAMESPACE},
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 cuttlefish tar"}, % How to use the plugin
            {opts, rebar_relx:opt_spec_list()},                   % list of options understood by the plugin
            {short_desc, "A rebar plugin"},
            {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("~s called", [?PROVIDER]),
    rebar3_cuttlefish_release_prv:do_build(tar, State).

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
