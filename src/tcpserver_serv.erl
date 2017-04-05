%%%-------------------------------------------------------------------
%%% @author Lustres
%%% @doc tcpserver connection handler
%%%
%%% @end
%%% Created : 03. Apr 2017 21:02
%%%-------------------------------------------------------------------
-module(tcpserver_serv).
-author("Lustres").

-behaviour(gen_server).

%% API
-export([start_link/3, change_count/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-type conn_tag() ::{reference(),pid()}.

-record(state, {count  ::non_neg_integer(),
                conns  ::gb_sets:set(conn_tag()),
                socket ::gen_tcp:socket(),
                sup_pid::pid()}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Sup ::pid(), Port ::inet:port_number(), Count ::non_neg_integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Sup, Port, Count) ->
  gen_server:start_link(?MODULE, [Sup, Port, Count], []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(change_count(NewCount ::non_neg_integer(), Pid ::pid()) -> ok).
change_count(NewCount, Pid) ->
  gen_server:cast(Pid, {change_count, NewCount}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Arg ::[term()]) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Sup, Port, Count]) ->
  {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}, {packet,line}]),
  ConnSupSpec = {conn_sup,
                 {tcpserver_conn_sup, start_link, [ListenSocket]},
                 permanent,
                 1000,
                 supervisor},
  {ok, SupPid} = supervisor:start_child(Sup, ConnSupSpec),
  self() ! init,
  {ok, #state{count  = Count,
              conns  = {gb_sets:new(), gb_sets:new()},
              socket = ListenSocket,
              sup_pid= SupPid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
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
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({change_count, NewCount}, State) when NewCount >= 0->
  {noreply, State#state{count=NewCount}};

handle_cast(Msg, State) ->
  tcpserver_lib:unknown_msg(cast, Msg),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(init, State = #state{count=Count, conns=Conns, sup_pid=SupPid}) when Count > 0 ->
  F = fun(_L, Set) ->
        ConnTag = new_connection(SupPid),
        gb_sets:insert(ConnTag, Set)
      end,
  NewConns = lists:foldl(F, Conns, lists:seq(1, Count)),
  {noreply, State#state{count=0, conns=NewConns}};

handle_info(init, State = #state{count=0}) ->
  {noreply, State};

handle_info({'DOWN', Ref, process, Pid, _Info},
            State = #state{count=Count, conns=Conns}) when Count > 0->
  {noreply, State#state{count=Count-1, conns=gb_sets:delete_any({Ref, Pid}, Conns)}};

handle_info({'DOWN', Ref, process, Pid, _Inf},
            State = #state{count=0, conns=Conns, sup_pid=SupPid}) ->
  {noreply, State#state{conns=gb_sets:insert(new_connection(SupPid),
                                             gb_sets:delete_any({Ref, Pid}, Conns))}};

handle_info(Info, State) ->
  tcpserver_lib:unknown_msg(info, Info),
  {noreply, State}.

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
terminate(_Reason, #state{socket=Socket}) ->
  gen_tcp:close(Socket).

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
%% Open a new connection on ConnSup
%%
%% @spec new_connection(ConnSup) -> ConnTag
%% @end
%%--------------------------------------------------------------------
-spec(new_connection(ConnSup ::pid()) -> conn_tag).
new_connection(ConnSup) ->
  {ok, Pid} = supervisor:start_child(ConnSup, []),
  Ref = erlang:monitor(process, Pid),
  {Ref, Pid}.
