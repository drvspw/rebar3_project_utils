-module(rebar3_project_utils).

-export([init/1, releases_to_build/1, read_relx_config/1, release_dir/1]).

-define(DEFAULT_PROVIDERS, [rebar3_check_deps_prv]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    %% get list of providers to initalize
    RelxConfig = rebar_state:get(State, relx, undefined),
    CuttlefishConfig = rebar_state:get(State, cuttlefish, [{enable, true}]), %% enable cuttlefish by default
    F1 = fun(P) -> add_release_providers(RelxConfig, P) end,
    F2 = fun(P) -> add_bump_provider(rebar_state:get(State, escript_name, undefined), P) end,
    F3 = fun(P) -> add_cuttlefish_provider(RelxConfig, CuttlefishConfig, P) end,
    Providers = pipe(?DEFAULT_PROVIDERS, [F1, F2, F3]),

    %% initialize providers
    FinalState = init_providers(State, Providers),

    %% return
    {ok, FinalState}.

releases_to_build(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    RelxConfig = read_relx_config(State),
    {ok, RelxState} = rlx_config:to_state(RelxConfig),
    AllReleases = highest_unique_releases(rlx_state:configured_releases(RelxState)),
    releases_to_build(AllReleases, Opts, RelxState).

read_relx_config(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    RelxConfigFile = proplists:get_value(config, Opts, []),
    read_relx_config(State, RelxConfigFile).

release_dir(State) ->
    DefaultRelDir = filename:join(rebar_dir:base_dir(State), "rel"),
    {Options, _} = rebar_state:command_parsed_args(State),
    proplists:get_value(output_dir, Options, DefaultRelDir).

%%======================================================================
%% Private functions
%%======================================================================
pipe(Init, Funs) ->
    lists:foldl( fun(F, State) -> F(State) end, Init, Funs).

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

add_cuttlefish_provider(undefined, _, Providers) ->
    %% not a release.
    Providers;

add_cuttlefish_provider(_, CuttlefishConfig, Providers) ->
    %% It is a release. Check if cuttlefish is enabled
    case proplists:get_value(enable, CuttlefishConfig, true) of
        true ->
            Providers ++ [rebar3_cuttlefish_release_prv, rebar3_cuttlefish_tar_prv, rebar3_ct_prv];
        _ ->
            Providers
    end.

read_relx_config(State, "") ->
    ConfigPath = filename:join([rebar_dir:root_dir(State), "relx.config"]),
    case rebar_state:get(State, relx, []) of
        [] ->
            rebar_api:debug("Configuring releases with relx.config", []),
            read_relx_config(State, ConfigPath);
        RebarConfig ->
            rebar_api:debug("Configuring releases the {relx, ...} entry"
                            " from rebar.config. Ignoring relx.config.", []),
            RebarConfig
    end;

read_relx_config(_State, ConfigFile) ->
    case file:consult(ConfigFile) of
        {ok, Config} ->
            rebar_api:debug("Configuring releases with: ~ts", [ConfigFile]),
            Config;
        {error, Reason} ->
            erlang:error({config_file, ConfigFile, Reason})
    end.

releases_to_build([_] = AllReleases, _Opts, _RelxState) ->
    AllReleases; %% if only one release configured

releases_to_build(AllReleases, Opts, _RelxState)->
    case {proplists:get_value(all, Opts, undefined),
          proplists:get_value(relnames, Opts, undefined)} of
        {undefined, undefined} ->
            case proplists:get_value(relname, Opts, undefined) of
                undefined ->
                    erlang:error("Must specify the name of the release to build when there are multiple releases in the config");
                R ->
                    case proplists:get_value(relvsn, Opts, undefined) of
                        undefined ->
                            filter_releases(AllReleases, R);
                        RelVsn ->
                            [{list_to_atom(R), RelVsn}]
                    end
            end;
        {true, _} ->
            AllReleases;
        {_, Filter} ->
            filter_releases(AllReleases, Filter)
    end.

filter_releases(Releases, Filter) ->
    WantReleases = [list_to_atom(Rel) || Rel <- string:split(Filter, ",", all)],
    [
     case proplists:lookup(Relname, Releases) of
         none -> erlang:error({release_not_found, Relname});
         Rel -> Rel
     end
     || Relname <- WantReleases
    ].

%% This function is copied verbatim from rebar3 source code.
%% takes a map of relx configured releases and returns a list of the highest
%% version for each unique release name
-spec highest_unique_releases(rlx_state:releases()) -> [{atom(), string() | undefined}].
highest_unique_releases(Releases) ->
    Unique = maps:fold(fun({Name, Vsn}, _, Acc) ->
                               update_map_if_higher(Name, Vsn, Acc)
                       end, #{}, Releases),
    maps:to_list(Unique).

update_map_if_higher(Name, Vsn, Acc) ->
    maps:update_with(Name, fun(Vsn1) ->
                                   case rlx_util:parsed_vsn_lte(rlx_util:parse_vsn(Vsn1),
                                                                rlx_util:parse_vsn(Vsn)) of
                                       true ->
                                           Vsn;
                                       false ->
                                           Vsn1
                                   end
                           end, Vsn, Acc).
