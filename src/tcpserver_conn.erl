%%%-------------------------------------------------------------------
%%% @author Lustres
%%% @doc
%%%
%%% @end
%%% Created : 03. Apr 2017 21:13
%%%-------------------------------------------------------------------
-module(tcpserver_conn).
-author("Lustres").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-record(state, {socket ::gen_tcp:socket()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(ListenSocket :: gen_tcp:socket()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ListenSocket) ->
  gen_server:start_link(?MODULE, [ListenSocket], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}}).
init([ListenSocket]) ->
  self() ! accept,
  {ok, #state{socket = ListenSocket}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, ok, NewState :: #state{}}).
handle_call(Request, _From, State) ->
  tcpserver_lib:unknown_msg(call, Request),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}}).
handle_cast(Request, State) ->
  tcpserver_lib:unknown_msg(cast, Request),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {stop, normal | {tcp_error, gen_tcp:socket(), term()}, NewState :: #state{}}).
handle_info(accept, State = #state{socket=ListenSocket}) ->
  {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
  send(AcceptSocket, "Hello", []),
  {noreply, State#state{socket = AcceptSocket}};

handle_info({tcp, _Port, <<"quit", _/binary>>}, S) ->
  gen_tcp:close(S#state.socket),
  {stop, normal, S};

handle_info({tcp, _Port, Msg}, State) when is_binary(Msg) ->
  send(State#state.socket, "received msg: ~p", [Msg]),
  {noreply, State};

handle_info({tcp, _Port, _Msg}, State) ->
  send(State#state.socket, "received unknown msg", []),
  {noreply, State};

handle_info({{tcp_close, _Socket}}, S) ->
  {stop, normal, S};

handle_info(Reason = {tcp_error, _Socket, _Reason}, S) ->
  {stop, Reason, S};

handle_info(Info, S) ->
  tcpserver_lib:unknown_msg(info, Info),
  {noreply, S}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, S) ->
  gen_tcp:close(S#state.socket).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send formatted message via socket
%% this function will set {active, once} socket after message sent
%% @spec send(Socket, Str, Args) -> ok
%% @end
%%--------------------------------------------------------------------
-spec(send(Socket :: gen_tcp:socket(), Str :: string(), Args :: [term()])
        -> ok).
send(Socket, Str, Args) ->
  ok = gen_tcp:send(Socket, io_lib:format(Str++"~n", Args)),
  ok = inet:setopts(Socket, [{active, once}]),
  ok.
