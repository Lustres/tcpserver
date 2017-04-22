%%%-------------------------------------------------------------------
%%% @author Lustres
%%% @doc
%%%
%%% @end
%%% Created : 05. Apr 2017 18:08
%%%-------------------------------------------------------------------
-module(tcpserver_manager).
-author("Lustres").

-behaviour(gen_server).

%% API
-export([start_link/0, listen/2, adjust/2, close/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {regs ::orddict:orddict(inet:port_number(), pid())}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Starts listen on port with given connections
%%
%% @end
%%--------------------------------------------------------------------
-spec(listen(Port ::inet:port_number(), Count::pos_integer()) -> ok).
listen(Port, Count) when Count > 0 ->
  gen_server:call(?SERVER, {listen, Port, Count}).

%%--------------------------------------------------------------------
%% @doc
%% Adjust the number of connections
%%
%% @end
%%--------------------------------------------------------------------
-spec(adjust(Port ::inet:port_number(), Count ::non_neg_integer()) -> ok).
adjust(Port, Count) when Count >= 0->
  gen_server:call(?SERVER, {adjust, Port, Count}).

%%--------------------------------------------------------------------
%% @doc
%% Adjust the number of connections
%%
%% @end
%%--------------------------------------------------------------------
-spec(close(Port ::inet:port_number()) -> ok).
close(Port) ->
  gen_server:cast(?SERVER, {close, Port}).

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
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  {ok, #state{regs=orddict:new()}}.

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
handle_call({listen, Port, Count}, _From, State = #state{regs = Regs}) ->
  {Reply, NewRegs} = case orddict:is_key(Port, Regs) of
    true  -> {already_started, Regs};
    false -> {ok, Pid} = supervisor:start_child(tcpserver_supersup, [Port, Count]),
             monitor(process, Pid),
             {ok, orddict:append(Port, Pid, Regs)}
  end,
  {reply, Reply, State#state{regs=NewRegs}};

handle_call({adjust, Port, Count}, _From, State = #state{regs = Regs}) ->
  Reply = case orddict:is_key(Port, Regs) of
    false  -> not_found;
    true -> tcpserver_serv:change_count(tcpserver_lib:serv_name(Port), Count), ok
  end,
  {reply, Reply, State};

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
handle_cast({close, Port}, State = #state{regs=Regs}) ->
  NewRegs = case orddict:find(Port, Regs) of
    error       -> Regs;
    {ok, [Pid]} -> ok = supervisor:terminate_child(tcpserver_supersup, Pid),
                   orddict:erase(Port, Regs)
  end,
  {noreply, State#state{regs=NewRegs}};

handle_cast(_Request, State) ->
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
handle_info(_Info, State) ->
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
terminate(_Reason, _State) ->
  ok.

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
