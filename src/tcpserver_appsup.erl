%%%-------------------------------------------------------------------
%%% @author Lustres
%%% @doc
%%%
%%% @end
%%% Created : 05. Apr 2017 22:32
%%%-------------------------------------------------------------------
-module(tcpserver_appsup).
-author("Lustres").

-behaviour(supervisor).

%% API
-export([start_link/0]).

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
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link(?MODULE, []).

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
init([]) ->
  MngSpec = {manager,
             {tcpserver_manager, start_link, []},
             permanent,
             2000,
             worker,
             [tcpserver_manager]},


  AppSpec = {appsup,
             {tcpserver_supersup, start_link, []},
             permanent,
             2000,
             supervisor,
             [tcpserver_supersup]},

  {ok, {{one_for_all, 0, 1}, [MngSpec, AppSpec]}}.
