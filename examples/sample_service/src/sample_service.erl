-module(sample_service).

%% api
-export([
         get_env/1,
         get_env/2,
         current_time/0,
         is_pid_alive/1,
         priv_dir/0,
         get_host_fqdn/0,
         log_level/1
        ]).

%% Application behaviour and callbacks
-behaviour(application).

-export([start/2, stop/1]).

%% supervisor behaviour and callbacks
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%-------------------------------------------------------------------
%% @doc sample_service public API
%% @end
%%%-------------------------------------------------------------------
get_env(EnvVar) ->
  get_env(EnvVar, undefined).

get_env(EnvVar, DefaultValue) ->
  {ok, App} = application:get_application(?MODULE),
  case application:get_env(App, EnvVar) of
    undefined -> DefaultValue;
    V -> V
  end.

priv_dir() ->
  case code:priv_dir(?MODULE) of
    {error, bad_name} ->
      logger:info("Couldn't find priv dir for the application, using ./priv~n"),
      "./priv";
    PrivDir -> filename:absname(PrivDir)
  end.

current_time() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega * 1000000 + Sec) * 1000000 + Micro.

is_pid_alive(Pid) when node(Pid) =:= node() ->
  is_process_alive(Pid);
is_pid_alive(Pid) ->
  lists:member(node(Pid), nodes())
    andalso (rpc:call(node(Pid), erlang, is_process_alive, [Pid]) =:= true).

get_host_fqdn() ->
  {ok, HostName} = inet:gethostname(),
  {ok, {hostent, FullHostName, _, inet, _, _}} = inet:gethostbyname(HostName),
  {ok, FullHostName}.

log_level(Level) ->
  logger:set_primary_config(level, Level).
%%--------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% @doc sample_service application callbacks
%% @end
%%%-------------------------------------------------------------------
start(_StartType, _StartArgs) ->
  {ok, Pid} = ?MODULE:start_link(),

  {ok, Pid}.

stop(_State) ->
  ok.
%%--------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% @doc sample_service top level supervisor.
%% @end
%%%-------------------------------------------------------------------
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%% child_spec() = #{ id => child_name,
%%                   start => {child_module, child_start_link_fun, []},
%%                   restart => permanent,
%%                   shutdown => 2000,
%%                   type => worker,
%%                   modules => [child_module]
%%                  }

init([]) ->
  RestartStrategy = {one_for_one, 4, 3600},
  Children = [], %% [ child_spec() ]
  {ok, {RestartStrategy, Children}}.

%%--------------------------------------------------------------------

%%=========================================================================
%% Private functions
%%=========================================================================

%%=========================================================================
%% Unit Test Suite
%%=========================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

suite_test_() ->
  [
   ?_assert(true)
  ].

-endif.
