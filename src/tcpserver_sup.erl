%%%-------------------------------------------------------------------
%%% @author Lustres
%%% @doc tcpserver port level supervisor.
%%% Combine tcpserver_serv with tcpserver_conn_sup together like one app
%%% @end
%%% Created : 03. Apr 2017 21:13
%%%-------------------------------------------------------------------
-module(tcpserver_sup).
-author("Lustres").

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Port, Count) ->
    supervisor:start_link(?MODULE, [Port, Count]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Port, Count]) ->
    ServSpec = {port_serv,
                {tcpserver_serv, start_link, [self(), Port, Count]},
                permanent,
                1000,
                worker,
                [tcpserver_serv]},
    {ok, { {one_for_all, 0, 1}, [ServSpec]} }.
