%%%-------------------------------------------------------------------
%%% @author Lustres
%%% @doc tcpserver port server
%%% Handle one port with tcpserver_conn_sup
%%% @end
%%% Created : 03. Apr 2017 21:02
%%%-------------------------------------------------------------------
-module(tcpserver_serv).
-author("Lustres").

-behaviour(gen_server).

%% API
-export([start_link/3, info/1, change_count/2]).

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
                downs  ::non_neg_integer(),
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
  gen_server:start_link({local, tcpserver_lib:serv_name(Port)}, ?MODULE, [Sup, Port, Count], []).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(info(Pid ::pid()) -> {Count::non_neg_integer(), Downs::non_neg_integer()}).
info(Pid) ->
  gen_server:call(Pid, info).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(change_count(Pid ::pid(), NewCount ::non_neg_integer()) -> ok).
change_count(Pid, NewCount) ->
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
  self() ! {start, Sup},
  {ok, #state{count  = Count,
              downs  = 0,
              conns  = gb_sets:new(),
              socket = ListenSocket}}.

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
handle_call(info, _From, State = #state{count=Count, downs=Downs}) ->
  {reply, {Count, Downs}, State};

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
  NewState = change_count_inner(NewCount, State),
  {noreply, NewState};

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
handle_info({start, Sup}, State = #state{socket=Socket}) ->
  ConnSupSpec = {conn_sup,
    {tcpserver_conn_sup, start_link, [Socket]},
    permanent,
    1000,
    supervisor,
    [tcpserver_supersup]},
  {ok, SupPid} = supervisor:start_child(Sup, ConnSupSpec),
  self() ! init,
  {noreply, State#state{sup_pid=SupPid}};

handle_info(init, State = #state{count=Count, conns=Conns, sup_pid=SupPid}) when Count > 0 ->
  {noreply, State#state{conns=add_connections(SupPid, Conns, Count)}};

handle_info(init, State = #state{count=0}) ->
  {noreply, State};

handle_info({'DOWN', Ref, process, Pid, _Info},
            State = #state{count=Count, downs=Downs, conns=Conns}) when Downs > 0->
  {noreply, State#state{count=Count-1, downs=Downs-1, conns=gb_sets:delete_any({Ref, Pid}, Conns)}};

handle_info({'DOWN', Ref, process, Pid, shutdown},
    State = #state{count=Count, conns=Conns}) ->
  {noreply, State#state{count=Count-1, conns=gb_sets:delete_any({Ref, Pid}, Conns)}};

handle_info({'DOWN', Ref, process, Pid, _Info},
            State = #state{downs=0, conns=Conns, sup_pid=SupPid}) ->
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add some connections in giving conns on supervisor
%%
%% @spec spawn_connections(ConnSup) -> ConnTag
%% @end
%%--------------------------------------------------------------------
-spec(add_connections(SupPid ::pid(),
                      Conns  ::gb_sets:set(conn_tag()),
                      Count  ::pos_integer())
                             -> gb_sets:set((conn_tag()))).
add_connections(SupPid, Conns, Count) ->
  F = fun(_L, Set) ->
        ConnTag = new_connection(SupPid),
        gb_sets:insert(ConnTag, Set)
      end,
  lists:foldl(F, Conns, lists:seq(1, Count)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Change count of connections
%%
%% @spec change_count(NewCount, State) -> State
%% @end
%%--------------------------------------------------------------------
-spec(change_count_inner(NewCount ::integer(), S ::#state{}) -> #state{}).
change_count_inner(NewCount, S = #state{count  = Count,
                                        conns  = Conns,
                                        sup_pid= SupPid}) when NewCount-Count > 0 ->
  NewConns = add_connections(SupPid, Conns, NewCount-Count),
  S#state{count=NewCount, conns=NewConns};

change_count_inner(NewCount, S = #state{count=Count}) when NewCount-Count < 0 ->
  S#state{downs=Count-NewCount};

change_count_inner(_, State) ->
  State.
