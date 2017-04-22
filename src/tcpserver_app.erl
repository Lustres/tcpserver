%%%-------------------------------------------------------------------
%%% @author Lustres
%%% @doc tcpserver public API
%%%
%%% @end
%%% Created : 03. Apr 2017 21:13
%%%-------------------------------------------------------------------
-module(tcpserver_app).
-author("Lustres").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([listen/2, adjust/2, close/1]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts listen on port with given connections
%%
%% @end
%%--------------------------------------------------------------------
-spec(listen(Port ::inet:port_number(), Count::pos_integer()) -> ok|already_started).
listen(Port, Count) when Count > 0 ->
  tcpserver_manager:listen(Port, Count).

%%--------------------------------------------------------------------
%% @doc
%% Adjust the number of connections
%%
%% @end
%%--------------------------------------------------------------------
-spec(adjust(Port ::inet:port_number(), Count ::non_neg_integer()) -> ok|not_found).
adjust(Port, Count) when Count >= 0->
  tcpserver_manager:adjust(Port, Count).

%%--------------------------------------------------------------------
%% @doc
%% Adjust the number of connections
%%
%% @end
%%--------------------------------------------------------------------
-spec(close(Port ::inet:port_number()) -> ok).
close(Port) ->
  tcpserver_manager:close(Port).

%%====================================================================
%% Application callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the app
%%
%% @end
%%--------------------------------------------------------------------

start(normal, _StartArgs) ->
  tcpserver_appsup:start_link().

%%--------------------------------------------------------------------
%% @doc
%% Stop the server
%%
%% @end
%%--------------------------------------------------------------------

stop(_State) ->
  ok.