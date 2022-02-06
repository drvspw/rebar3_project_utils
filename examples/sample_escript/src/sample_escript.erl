-module(sample_escript).

%% API exports
-export([main/1]).

-define(APP_NAME, ?MODULE).

%% commands
-define(CMD_VERSION, "version").
-define(CMD_HELP, "help").

%%====================================================================
%% API functions
%%====================================================================
main(Args) ->
    {ok, _} = application:ensure_all_started(?APP_NAME),
    OptSpecList = option_spec_list(),
    Cmd = getopt:parse(OptSpecList, Args),
    exec(Cmd).

%%====================================================================
%% Internal functions
%%====================================================================
exec({ok, {[], []}}) ->
    help();

exec({ok, {_Options, [?CMD_VERSION]}}) ->
    version();

exec({ok, {_Options, [?CMD_HELP]}}) ->
    help();

exec({ok, {Options, [Command]}}) ->
    try
        execute_command(Command, maps:from_list(Options))
    catch
        _ : Reason ->
            io:format("ERROR: ~p~n", [Reason]),
            erlang:halt(1)
    end;

exec({ok, {_, _}}) ->
    help();

exec({error, {Reason, Data}}) ->
    io:format("~s ~p~n", [Reason, Data]),
    help(),
    erlang:halt(1).

execute_command(_, _) ->
    help().

%% options spec list
%% {Key, $Character, "StringName", Spec, HelpText}
%% {port, $p, "port", {integer, 5432}, "Database server port"}
option_spec_map() ->
    #{
      %% cmd => Options
     }.

option_spec_list() ->
    Map = option_spec_map(),
    FoldFn = fun(_, Options, Acc) ->
                     Acc ++ Options
             end,
    maps:fold(FoldFn, [], Map).

help() ->
    %% Print version
    version(),

    %% Print usage
    AppName = atom_to_list(?APP_NAME),
    OptSpecMap = option_spec_map(),
    maps:foreach( fun(Cmd, Options) ->
                          getopt:usage(Options, AppName, Cmd, [])
                  end, OptSpecMap).

version() ->
    case lists:keyfind(?APP_NAME, 1, application:loaded_applications()) of
        {_, _, Ver} ->
            io:format("Version: ~s\n", [Ver]);
        false ->
            io:format("ERROR: Unable to get version~n"),
            erlang:halt(1)
    end.
