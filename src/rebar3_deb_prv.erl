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
    Releases = rebar3_project_utils:releases_to_build(State),

    %% Check if debian configuration is in state
    DebConfig = read_deb_config(State), %%read_deb_config(State),

    %% Get the directories
    RelDir = rebar3_project_utils:release_dir(State),
    DebDir = filename:join(rebar_dir:base_dir(State), "deb"),

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
                           rebar_api:info("Creating debian package ~s.deb", [filename:basename(ReleaseDebDir)]),
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
