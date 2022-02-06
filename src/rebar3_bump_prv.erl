-module(rebar3_bump_prv).

-export([init/1, do/1, format_error/1, format_error/2]).

-define(PROVIDER, bump).
-define(DEPS, []).

-define(SHORT_DESC, "A rebar3 plugin similar to ruby bump gem.").
-define(DESC, "A rebar3 plugin similar to ruby bump gem. \n"
              "Configure the base version in your bump.config. \n"
              "    {version, \"MAJOR.MINOR.PATCH\"}\n"
              "Set `vsn` attribute in .app.src and relx config to \n"
              "     {cmd, \"rebar3 bump\"}. \n"
              "The plugin can then be invoked to update version. The \n"
              "plugin automatically appends git hash to version.\n").

-define(DEFAULT_VERSION, "0.0.0").

-define(ARG_MAJOR, "major").
-define(ARG_MINOR, "minor").
-define(ARG_PATCH, "patch").
-define(ARG_PRE, "pre").

-define(ALPHA, alpha).
-define(BETA, beta).
-define(RC, rc).

-define(BUMP_FILE, "bump.config").

-define(VERSION_STATE_KEY, version).

%% git commands
-define(GIT_COMMIT_HASH, "git rev-parse --short HEAD 2>/dev/null").
-define(GIT_COMMIT_COUNT, "git log --oneline 2>/dev/null | wc -l | tr -d ' '").

-type pre() :: alpha | beta | rc.

-type version_tuple() :: {integer(), integer(), integer()} | {integer(), integer(), integer(), pre()}.

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},             %% The 'user friendly' name of the task
            {module, ?MODULE},             %% The module implementation of the task
            {bare, true},                  %% The task can be run by the user, always true
            {deps, ?DEPS},                 %% The list of dependencies
            {example, "rebar3 bump [major|minor|patch|pre]"},      %% How to use the plugin
            {opts, []},                    %% list of options understood by the plugin
            {short_desc, ?SHORT_DESC},
            {desc, ?DESC}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Cmd = proplists:get_value(task, Args),
    BumpFile = bump_file_path(State),
    handle_bump_command(Cmd, BumpFile, State).

-spec handle_bump_command(string(), string(), rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
handle_bump_command(undefined, BumpFile, State) ->
    Version = parse_version(BumpFile),
    Count = git_count(),
    Hash = git_hash(),
    VersionStr = version_string(Version),
    Format = case Version of
        {_, _, _} ->
            "~s-~p.~s";
        _ ->
            "~s.~p.~s"
    end,
    io:format(Format, [VersionStr, Count, Hash]),
    {ok, State};

handle_bump_command(Cmd, BumpFile, State) ->
    try
        %% parse version
        Version = parse_version(BumpFile),

        %% bump version
        NewVersion = bump(Cmd, Version),
        VersionStr = version_string(NewVersion),

        %% write to bump.config
        Data = io_lib:format("~tp.~n", [{?VERSION_STATE_KEY, lists:flatten(VersionStr)}]),
        ok = file:write_file(BumpFile, Data),

        %% display success
        rebar_api:info("Bumped version to ~s", [VersionStr]),

        %% return state
        {ok, State}
    catch
        error : Reason ->
            {error, format_error("~s", [Reason])};
        Error : Reason ->
            {error, format_error("~p~n~p", [Error, Reason])}
    end.

-spec format_error(iolist(), any()) ->  iolist().
format_error(Format, Reason) ->
    io_lib:format(Format, Reason).

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

-spec get_version(string()) -> iolist().
get_version(BumpFile) ->
    case file:consult(BumpFile) of
        {ok, Data} ->
            proplists:get_value(?VERSION_STATE_KEY, Data, ?DEFAULT_VERSION);
        {error, enoent} ->
            ?DEFAULT_VERSION;
        {error, Reason} ->
            ErrorStr = file:format_error(Reason),
            erlang:error(format_error("Error reading ~s! ~s", [BumpFile, ErrorStr]))
    end.

-spec parse_version(string()) -> version_tuple().
parse_version(BumpFile) ->
    Version = get_version(BumpFile),
    Values = re:split(Version, "\\.|-", [{return, list}]),
    case Values of
        [MJ, MN, P] ->
            {list_to_integer(MJ), list_to_integer(MN), list_to_integer(P)};
        [MJ, MN, P, PRE] ->
            {list_to_integer(MJ), list_to_integer(MN), list_to_integer(P), list_to_existing_atom(PRE)};
        _ ->
            erlang:error(format_error("Invalid Version: ~s", [Version]))
    end.

-spec bump(string(), version_tuple()) -> version_tuple().
bump(?ARG_MAJOR, {M, _, _}) ->
    {M+1, 0, 0};
bump(?ARG_MAJOR, {M, _, _, _}) ->
    {M+1, 0, 0};
bump(?ARG_MINOR, {MJ, MN, _}) ->
    {MJ, MN+1, 0};
bump(?ARG_MINOR, {MJ, MN, _, _}) ->
    {MJ, MN+1, 0};
bump(?ARG_PATCH, {MJ, MN, P}) ->
    {MJ, MN, P+1};
bump(?ARG_PATCH, {MJ, MN, P, _}) ->
    {MJ, MN, P+1};
bump(?ARG_PRE, {MJ, MN, P}) ->
    {MJ, MN, P, ?ALPHA};
bump(?ARG_PRE, {MJ, MN, P, ?ALPHA}) ->
    {MJ, MN, P, ?BETA};
bump(?ARG_PRE, {MJ, MN, P, ?BETA}) ->
    {MJ, MN, P, ?RC};
bump(?ARG_PRE, {MJ, MN, P, ?RC}) ->
    {MJ, MN, P};
bump(Cmd, _) ->
    erlang:error(format_error("Unknown Command: '~s'", [Cmd])).

-spec version_string(version_tuple()) -> iolist().
version_string({MJ, MN, P}) ->
    io_lib:format("~p.~p.~p", [MJ, MN, P]);
version_string({MJ, MN, P, PRE}) ->
    io_lib:format("~p.~p.~p-~s", [MJ, MN, P, PRE]).

-spec bump_file_path(string()) -> string().
bump_file_path(State) ->
    Dir = rebar_state:dir(State),
    filename:join([Dir, ?BUMP_FILE]).

-spec git_hash() -> string().
git_hash() ->
  case os_cmd(?GIT_COMMIT_HASH, [no_halt]) of
    [] ->
      "0000000";
    Hash ->
      Hash
  end.

-spec git_count() -> integer().
git_count() ->
  list_to_integer(os_cmd(?GIT_COMMIT_COUNT)).

os_cmd(Command) ->
  os_cmd(Command, []).

os_cmd(Command, Options) ->
  Port = open_port({spawn, Command}, [stream, in, eof, hide, exit_status]),
  Output = capture_cmd_output(Port, []),
  parse_cmd_output(Output, Options).

parse_cmd_output({0, Out}, _) ->
  %% Command completed successfully
  case lists:reverse(Out) of
    [$\n|Rest] ->
      lists:reverse(Rest);

    _ ->
      Out
  end;

parse_cmd_output({_ExitCode, _}, [no_halt]) ->
  %% Command failed with ExitCode
  "";

parse_cmd_output({ExitCode, _}, _) ->
  %% Command failed with ExitCode
  erlang:halt(ExitCode).


capture_cmd_output(Port, Sofar) ->
  receive
    {Port, {data, Bytes}} ->
      capture_cmd_output(Port, [Sofar|Bytes]);

    {Port, eof} ->
      Port ! {self(), close},
      receive
        {Port, closed} ->
          true
      end,
      receive
        {'EXIT',  Port,  _} ->
          ok
      after 1 ->              % force context switch
          ok
      end,
      ExitCode =
        receive
          {Port, {exit_status, Code}} ->
            Code
        end,
      {ExitCode, lists:flatten(Sofar)}
  end.
