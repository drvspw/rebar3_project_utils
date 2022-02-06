-module(rebar3_deb_prv).

-export([init/1, do/1, format_error/1]).

-include_lib("kernel/include/file.hrl").

-define(PROVIDER, debian).
-define(DEPS, [tar]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 debian"}, % How to use the plugin
            {opts, rebar_relx:opt_spec_list()},                   % list of options understood by the plugin
            {short_desc, "A rebar plugin to package release as a debian package."},
            {desc, "A rebar plugin to package release as a debian package."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    %% Get the releases
    {Opts, _} = rebar_state:command_parsed_args(State),
    RelxConfigFile = proplists:get_value(config, Opts, []),
    RelxConfig = read_relx_config(State, RelxConfigFile),
    {ok, RelxState} = rlx_config:to_state(RelxConfig),
    AllReleases = highest_unique_releases(rlx_state:configured_releases(RelxState)),
    Releases = releases_to_build(AllReleases, Opts, RelxState),

    %% Check if debian configuration is in state
    DebConfig = read_deb_config(State), %%read_deb_config(State),

    %% Get the directories
    BaseDir = rebar_dir:base_dir(State),
    RelDir = filename:join(BaseDir, "rel"),
    DebDir = filename:join(BaseDir, "deb"),

    %% create deb directory for each release
    lists:foreach( fun({Name, Vsn}) ->
                           ReleaseTarName = release_tar_name(Name, Vsn),
                           ReleaseDebDir = filename:join([DebDir, deb_package_dir(Name, Vsn)]),

                           %% get install dir from config
                           ConfigInstallDir = case proplists:get_value(install_dir, DebConfig, "") of
                                                  "" ->
                                                      filename:join(["opt", Name]);
                                                  [$/|Dir] -> %% ignore leading /
                                                      Dir;
                                                  Dir ->
                                                      Dir
                                              end,
                           InstallDir = filename:join([ReleaseDebDir, ConfigInstallDir, Name]),

                           %% extract release tar
                           ok = rlx_file_utils:mkdir_p(InstallDir),
                           ok = erl_tar:extract(filename:join([RelDir, Name, ReleaseTarName]), [compressed, {cwd, InstallDir}]),

                           %% copy control files
                           case proplists:get_value(control_files_dir, DebConfig) of
                               undefined ->
                                   erlang:error("control_files_dir is required");
                               "" ->
                                   erlang:error("control_files_dir is required");
                               ControlFilesDir ->
                                   TemplateVars = #{"package" => deb_package(Name),
                                                    "version" => Vsn,
                                                    "name" => Name},
                                   PkgControlDir = filename:join([ReleaseDebDir, "DEBIAN"]),
                                   {ok, Files} = file:list_dir(ControlFilesDir),
                                   rlx_file_utils:mkdir_p(PkgControlDir),
                                   lists:foreach( fun(File) ->
                                                          FileName = filename:join([ControlFilesDir, File]),
                                                          {ok, Data} = file:read_file(FileName),
                                                          {ok, #file_info{mode = Mode}} = file:read_file_info(FileName),
                                                          RenderedData = bbmustache:render(Data, TemplateVars),
                                                          OutFileName = filename:join([PkgControlDir, File]),
                                                          ok = file:write_file(OutFileName, RenderedData),
                                                          {ok, OutFileInfo} = file:read_file_info(OutFileName),
                                                          NewOutFileInfo = OutFileInfo#file_info{mode = Mode},
                                                          ok = file:write_file_info(OutFileName, NewOutFileInfo)
                                                  end, Files)
                           end,

                           %% copy oother source dirs
                           lists:foreach(fun(Dir) ->
                                                 ok = rlx_file_utils:copy(Dir, ReleaseDebDir, [recursive])
                                         end, proplists:get_value(source_dirs, DebConfig, [])),

                           %% copy systemd unit file if configured
                           SystemdDir = filename:join([ReleaseDebDir, "usr/lib/systemd/system"]),
                           ok = rlx_file_utils:mkdir_p(SystemdDir),
                           lists:foreach(fun(File) ->
                                                 ok = rlx_file_utils:copy(File, filename:join([SystemdDir, filename:basename(File)]))
                                         end, proplists:get_value(systemd_unit_files, DebConfig, [])),

                           %% Build debian package
                           Cmd = lists:flatten(io_lib:format("dpkg-deb --build ~s ~s", [ReleaseDebDir, DebDir])),
                           rlx_util:sh(Cmd),

                           %% success
                           rebar_api:info("Debain package successfully created: ~s.deb", [ReleaseDebDir])
                   end, Releases),
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

read_deb_config(State) ->
    rebar_state:get(State, deb, []).

release_tar_name(Name, Vsn) ->
    lists:flatten(io_lib:format("~s-~s.tar.gz", [Name, Vsn])).

deb_package(Name) when is_atom(Name) ->
    deb_package( atom_to_list(Name) );
deb_package(Name) ->
    re:replace(Name, "_", "-", [global, {return, list}]).

deb_package_dir(Name, Vsn) ->
    lists:flatten(io_lib:format("~s_~s_amd64", [deb_package(Name), Vsn])).

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
