%%%-------------------------------------------------------------------
%%% @author Lustres
%% @doc tcpserver top level supervisor.
%%%
%%% @end
%%% Created : 03. Apr 2017 21:13
%%%-------------------------------------------------------------------
-module(tcpserver_sup).
-author("Lustres").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link(?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([Port, Count]) ->
    ServSpec = {port_serv,
                {tcpserver_serv, start_link, [self(), Port, Count]},
                permanent,
                1000,
                worker},
    {ok, { {reset_for_all, 4, 3600}, [ServSpec]} }.
