-module(rebar3_check_deps_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, 'check-deps').
-define(DEPS, [install_deps]).

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
                               {example, "rebar3 check-deps"}, % How to use the plugin
                               {opts, []},                   % list of options understood by the plugin
                               {short_desc, "A rebar plugin"},
                               {desc, "A rebar plugin"}
                              ]),
  {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
  %% Get apps
  ProjectApps = rebar_state:project_apps(State),

  %% Get Top level Deps
  Deps = rebar_state:get(State, deps, []),

  check_deps(State, ProjectApps, Deps).

-spec check_deps(rebar_state:t(), list(), list()) -> {ok, rebar_state:t()} | {error, string()}.
check_deps(State, [AppInfo], Deps) ->
  Appname = rebar_utils:to_list(rebar_app_info:name(AppInfo)),
  Depnames = [element(1, Dep) || Dep <- Deps],

  rebar_api:info("Checking runtime dependencies of ~s", [Appname]),

  ProjectDir = rebar_app_info:dir(AppInfo),
  AppSrcFile = filename:join([ProjectDir, "src", Appname ++ ".app.src"]),
  {ok, [AppSpec]} = file:consult(AppSrcFile),

  %% verify apps spec has the correct format
  {application, _, AppSpecOptions} = AppSpec,
  Applications = proplists:get_value(applications, AppSpecOptions, []),
  IncludedApplications = proplists:get_value(included_applications, AppSpecOptions, []),

  case check_deps(Depnames, Applications ++ IncludedApplications) of
    true ->
      {ok, State};
    false ->
      {error, "Some Dependencies not included in app.src"}
  end;

check_deps(_State, _, _) ->
  {error, "Does not work on umbrella projects!"}.


check_deps(Depnames, AppSrcApps) ->
  lists:foldl( fun(Depname, Flag) ->
                   IsIncluded = lists:member(Depname, AppSrcApps),
                   report_check_result(IsIncluded, Depname),
                   Flag andalso IsIncluded
                 end, true, Depnames).

report_check_result(true, Depname) ->
  rebar_api:info("~p is included in app.src", [Depname]);

report_check_result(false, Depname) ->
  rebar_api:error("~p is NOT included in app.src", [Depname]).

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).
