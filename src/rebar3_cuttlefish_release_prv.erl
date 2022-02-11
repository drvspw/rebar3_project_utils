%% Copyright Notice
%%   Some of the code in this file is taken from https://github.com/vernemq/rebar3_cuttlefish repo
%%
-module(rebar3_cuttlefish_release_prv).

-export([init/1, do/1, format_error/1, do_build/2]).

-define(PROVIDER, release).
-define(NAMESPACE, default).
-define(DEPS, [{?NAMESPACE, compile}]).
-define(SCHEMA_IDX_START, 10).

-define(PLUGIN_NAME, rebar3_project_utils).

-define(SCHEMA_DIR, "share/schema").
-define(HOOK_DIR, "bin/hooks").
-define(CUTTLEFISH_EXE, "bin/cuttlefish").
-define(GENERATED_CONF_DIR, "generated.conf").
-define(ETC_DIR, "etc").

-define(PRE_START_HOOK_TEMPLATE, "cuttlefish_pre_start_hook").
-define(PRE_START_HOOK_FILE, "cuttlefish_pre_start").

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
            {example, "rebar3 cuttlefish release"}, % How to use the plugin
            {opts, rebar_relx:opt_spec_list()},                   % list of options understood by the plugin
            {short_desc, "A rebar plugin"},
            {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    %% Get relx config
    RelxConfig0 = rebar3_project_utils:read_relx_config(State),

    %% Get releases to build
    ReleasesToBuild = rebar3_project_utils:releases_to_build(State),

    %% get overlays
    Overlays0 = proplists:get_value(overlay, RelxConfig0, []),

    %% pre start hook template file
    PrivTemplateFile = filename:join([code:priv_dir(?PLUGIN_NAME), ?PRE_START_HOOK_TEMPLATE]),
    ReleasePreStartHookFile = filename:join([?HOOK_DIR, ?PRE_START_HOOK_FILE]),

    %% Get cuttlefish executable
    CuttlefishBin = case filelib:is_regular(filename:join([rebar_dir:base_dir(State), "bin", "cuttlefish"])) of
                 true ->
                     filename:join([rebar_dir:base_dir(State), "bin", "cuttlefish"]);
                 false ->
                     case filelib:wildcard(filename:join(["_checkouts", "cuttlefish*", "cuttlefish"])) ++
                         filelib:wildcard(filename:join(["_build", "*", "bin", "cuttlefish"])) of
                         [C | _] ->
                             C;
                         [] ->
                             throw({no_cuttlefish_escript, rebar_dir:base_dir(State)})
                     end
             end,

    %% add syntax_tools to to release. requried for cuttlefish in prod mode
    RelxConfig = configure_release_for_cuttlefish(RelxConfig0),

    %% overlay to copy schema files to share/schemas folder
    CuttlefishConf = rebar_state:get(State, cuttlefish, []),
    SchemaOrder = proplists:get_value(schema_order, CuttlefishConf, []),
    AllSchemas = case proplists:get_value(schema_discovery, CuttlefishConf, true) of
                          false ->
                              [];
                          _ ->
                              Deps = rebar_state:all_deps(State),
                              Apps = rebar_state:project_apps(State),
                              find_all_cuttlefish_schema_files(Deps ++ Apps)
                      end,

    %% overly all schema files
    SchemaOverlays = schema_overlays(Overlays0, AllSchemas, SchemaOrder),

    %% update the overlays to create
    Overlays = [{mkdir, ?SCHEMA_DIR}, {mkdir, ?HOOK_DIR}, {mkdir, ?ETC_DIR},
                {mkdir, ?GENERATED_CONF_DIR}, {copy, CuttlefishBin, ?CUTTLEFISH_EXE},
                {template, PrivTemplateFile, ReleasePreStartHookFile} | Overlays0] ++ SchemaOverlays,

    %% replace overlays in relx config
    OverlayRelxConfig = lists:keydelete(overlay, 1, RelxConfig) ++ [{overlay, Overlays}],
    FinalRelxConfig = add_pre_start_hook_config(OverlayRelxConfig),

    %% replace relx config in rebar state
    PreReleaseState = rebar_state:set(State, relx, FinalRelxConfig),

    %% release
    ReleaseResult = do_build(?PROVIDER, PreReleaseState),

    %% generate conf file for each release
    RelDir = rebar3_project_utils:release_dir(State),
    lists:foreach( fun({Name, _Vsn}) ->
                           ReleaseSchemaDir = filename:join([RelDir, Name, ?SCHEMA_DIR]),
                           ReleaseEtcDir = filename:join([RelDir, Name, ?ETC_DIR]),
                           ReleaseSchemas = find_cuttlefish_schema_files(ReleaseSchemaDir),
                           case cuttlefish_schema:files(ReleaseSchemas) of
                               {errorlist, _Es} ->
                                   %% These errors were already printed
                                   {error, "bad cuttlefish schemas"};
                               {_Translations, Mappings, _Validators} ->
                                   ConfFilename = filename:join([ReleaseEtcDir, Name]) ++ ".conf",
                                   cuttlefish_conf:generate_file(Mappings, ConfFilename)
                           end
                   end, ReleasesToBuild),

    %% return
    ReleaseResult.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

do_build(Provider, State) ->
    rebar_relx:do(Provider, State).

add_pre_start_hook_config(RelxConfig) ->
    StartupHooks = proplists:get_value(extended_start_script_hooks, RelxConfig, []),
    PreStartHooks = proplists:get_value(pre_start, StartupHooks, []),

    %% add cuttlefish pre start hook
    PreStartHookFile = filename:join(["hooks", ?PRE_START_HOOK_FILE]),
    UpdatedPreStartHooks = [{custom, PreStartHookFile}|PreStartHooks],
    UpdatedStartupHooks = lists:keydelete(pre_start, 1, StartupHooks) ++ [{pre_start, UpdatedPreStartHooks}],

    %% update relx config
    lists:keydelete(extended_start_script_hooks, 1, RelxConfig) ++ [{extended_start_script_hooks, UpdatedStartupHooks}].

configure_release_for_cuttlefish(RelxConfig) ->
    Fn = fun({release, _, _}) -> true;
            (_) -> false
         end,
    {Releases, Others} = lists:splitwith(Fn, RelxConfig),
    NewReleases = [{release, Rel, Apps ++ [syntax_tools]} || {release, Rel, Apps} <- Releases],
    NewReleases ++ Others.

find_all_cuttlefish_schema_files(Apps) ->
    SchemaFromDeps = lists:flatmap(fun(App) ->
                                           Dir = rebar_app_info:dir(App),
                                           find_cuttlefish_schema_files(filename:join([Dir, "{priv,schema}"]))
                                   end, Apps),
    SchemaFromPlugin = find_cuttlefish_schema_files(code:priv_dir(?PLUGIN_NAME)),
    SchemaFromDeps ++ SchemaFromPlugin.

find_cuttlefish_schema_files(Dir) ->
    filelib:wildcard(filename:join([Dir, "*.schema"])).

schema_overlays(Overlays, AllSchemas, SchemaOrder) ->
    OverlaySourceFiles = [ list_to_binary(Src) || {_, Src, _} <- Overlays ],
    [begin
         FileName = create_filename(Schema, SchemaOrder),
         {template, Schema, filename:join([?SCHEMA_DIR, FileName])}
     end || Schema <- AllSchemas, not is_overlay_source(list_to_binary(Schema), OverlaySourceFiles)].

is_overlay_source(Schema, OverlaySources) ->
    Suffixes = [{byte_size(OverlaySrc),
                 binary:longest_common_suffix(
                   [Schema, OverlaySrc])} || OverlaySrc <- OverlaySources],
    [L || {L, L} <- Suffixes] =/= [].

create_filename(Schema, []) ->
    filename:basename(Schema);

create_filename(Schema, OrderSchemas) ->
    SchemaBaseName = list_to_atom(filename:basename(Schema, ".schema")),
    IndexOrderSchemas = lists:zip(OrderSchemas, lists:seq(?SCHEMA_IDX_START, length(OrderSchemas) + (?SCHEMA_IDX_START - 1))),
    Index = lists:keyfind(SchemaBaseName, 1, IndexOrderSchemas),
    maybe_index_filename(Schema, Index).

maybe_index_filename(Schema, false) ->
    filename:basename(Schema);
maybe_index_filename(Schema, []) ->
    filename:basename(Schema);
maybe_index_filename(Schema, {_, Index}) ->
    integer_to_list(Index) ++ "-" ++ filename:basename(Schema).
