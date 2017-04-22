%%%-------------------------------------------------------------------
%%% @author Lustres
%%% @doc
%%%
%%% @end
%%% Created : 05. Apr 2017 12:38
%%%-------------------------------------------------------------------
-module(tcpserver_lib).
-author("Lustres").

%% API
-export([unknown_msg/2, serv_name/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Handle unknown massage
%% use system log
%% @spec unknown_msg(Type, Msg) -> ok
%% @end
%%--------------------------------------------------------------------
-spec(unknown_msg(Type :: call | cast | info | term(), Msg :: any()) -> ok).
unknown_msg(Type, Msg) ->
  error_logger:warning_msg("received unknown ~p: ~p~n", [Type, Msg]).

%%--------------------------------------------------------------------
%% @doc
%% generate server name by port
%%
%% @spec serv_name(Port) -> Name
%% @end
%%--------------------------------------------------------------------
-spec(serv_name(Port ::inet:port_number()) -> atom()).
serv_name(Port) ->
  list_to_atom("serv" ++ integer_to_list(Port)).
