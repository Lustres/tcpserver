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
-export([unknown_msg/2]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handle unknown massage
%% use system log
%% @spec unknown_msg(Type, Msg) -> ok
%% @end
%%--------------------------------------------------------------------
-spec(unknown_msg(Type :: call | cast | info | term(), Msg :: any()) -> ok).
unknown_msg(Type, Msg) ->
  error_logger:warning_msg("received unknown ~p: ~p~n", [Type, Msg]).
