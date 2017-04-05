%%%-------------------------------------------------------------------
%%% @author Lustres
%%% @doc supervisor of tcpserver_conn
%%%
%%% @end
%%% Created : 04. Apr 2017 14:09
%%%-------------------------------------------------------------------
-module(tcpserver_conn_sup).
-author("Lustres").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(ListenSocket ::inet:socket()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ListenSocket) ->
  supervisor:start_link(?MODULE, [ListenSocket]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]
  }} |
  ignore |
  {error, Reason :: term()}).
init([ListenSocket]) ->
  ChildSpec = {conn_handler,
               {tcpserver_conn, start_link, [ListenSocket]},
               permanent,
               10,
               worker,
               [tcpserver_conn]},

  {ok, { {simple_one_for_one, 10000, 1}, [ChildSpec]}}.
