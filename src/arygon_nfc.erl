%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    NFC reader support
%%% @end
%%% Created : 19 Jun 2012 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(arygon_nfc).

-behaviour(gen_server).

%% API
-export([start/0]).
-export([start_link/0]).
-export([start_link/1]).

-export([subscribe/0, unsubscribe/1, setopts/1, getopts/1, stop/0]).
%% debug
-export([client/0]).
-export([command/1]).
-export([execute/1]).
-export([test/3, test/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(subscription,
	{
	  pid :: pid(),
	  mon :: reference()
	}).

-record(state,
	{
	  device :: string(),
	  uart :: port(),
	  baud :: non_neg_integer(),
	  reopen_timer :: undefined | reference(),
	  reopen_ival :: timeout(),
	  card = "",  %% current card
	  cmd_list = [],
	  array    = undefined,
	  sub_list=[] :: [#subscription{}]
	}).

-define(is_page(I), ((I >= 16#00) andalso (I =< 16#FF))).

%%%===================================================================
%%% API
%%%===================================================================
subscribe() ->
    gen_server:call(?SERVER, {subscribe, self()}).

unsubscribe(Ref) ->
    gen_server:call(?SERVER, {unsubscribe, Ref}).

command(Data) ->
    gen_server:call(?SERVER, {command, Data}).	

execute(Cmds) ->
    gen_server:call(?SERVER, {execute, Cmds}).

client() ->
    spawn(fun() ->
		  {ok,Mon} = subscribe(),
		  client_loop(Mon, "", undefined)
	  end).

%% At least Page 6,15 are pure DATA 6-39 Ultralight C  (reserved 40-47)
client_loop(Mon,CardID,Ref) ->
    Page = 6,
    receive
	{timeout,Ref,{next,CardID}} ->
	    lager:debug("NEXT"),
	    command("0s"),
	    Ref1 = erlang:start_timer(2000,self(),{clear,CardID}),
	    client_loop(Mon,CardID,Ref1);

	{timeout,Ref,{clear,CardID}} ->
	    lager:debug("CLEAR"),
	    client_loop(Mon,"",undefined);

	{timeout,_Ref,_} -> %% ignore old timeouts
	    client_loop(Mon,CardID,Ref);
	    
	{nfc,Mon,{select,CardID}} ->
	    lager:debug("SELECT OLD"),
	    command("0s"),
	    client_loop(Mon,CardID,Ref);

	{nfc,Mon,{select,NewCardID}} ->
	    lager:debug("SELECT NEW: ~s", [NewCardID]),
	    stop_timer(Ref),
	    execute([{load,Page},
		     {ite, {gt,Page,0},
		      [{update,Page,-1},{save,Page},ok],
		      [error]}
		    ]),
	    client_loop(Mon,NewCardID,undefined);

	{nfc,Mon,{ok,CardID}} ->
	    lager:debug("CARD OK: ~s", [CardID]),
	    play(ok),
	    Ref1 = erlang:start_timer(1000,self(),{next,CardID}),
	    client_loop(Mon,CardID,Ref1);

	{nfc,Mon,{error,CardID}} ->
	    lager:debug("CARD ERROR: ~s", [CardID]),
	    play(error),
	    Ref1 = erlang:start_timer(1000,self(),{next,CardID}),
	    client_loop(Mon,CardID,Ref1);

	{nfc,Mon,device_open} ->
	    lager:debug("DEVICE OPEN"),
	    command("0s"),
	    client_loop(Mon,"",undefined);

	{nfc,Mon,device_closed} ->
	    lager:debug("DEVICE CLOSE"),
	    if CardID =:= "" ->
		    ok;
	       true ->
		    play(error)
	    end,
	    stop_timer(Ref),
	    client_loop(Mon,"",undefined);

	_Message ->
	    lager:debug("client: got ~p\n", [_Message]),
	    client_loop(Mon,CardID,Ref)
    end.
    
stop_timer(undefined) ->
    ok;
stop_timer(Ref) ->
    erlang:cancel_timer(Ref).

	


test(init,Page,Value) when ?is_page(Page), is_integer(Value) ->
    execute([{set,Page,Value},{save,Page}]).

test(update,Page) when ?is_page(Page) ->
    execute([{load,Page},
	     {ite, {gt,Page,0},
	      [{update,Page,-1},{save,Page},ok],
	      [error]}
	    ]).

	
setopts(Opts) ->
    gen_server:call(?SERVER, {setopts,Opts}).

getopts(Opts) ->
    gen_server:call(?SERVER, {getopts,Opts}).

start() ->
    application:start(?MODULE).

stop() ->
    gen_server:call(?SERVER, stop).    
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    case application:get_env(arygon_nfc, device) of
	{ok,Device} when is_list(Device) ->
	    start_link([{device,Device}]);
	undefined ->
	    start_link([]);
	Other ->
	    lager:critical("warning: bad device value given [~p]", [Other])
    end.

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

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
init(Args) ->
    lager:start(),  %% ok testing, remain or go?
    Device = proplists:get_value(device, Args, "/dev/ttyUSB0"),
    Baud = proplists:get_value(baud, Args, 9600),
    Reopen_Timer = erlang:start_timer(100, self(), open_device),
    Reopen_Ival = proplists:get_value(reopen_ival, Args, 20*1000),
    case proplists:get_bool(debug, Args) of
	true ->
	    lager:set_loglevel(lager_console_backend, debug);
	_ ->
	    ok
    end,
    {ok,
     #state{
       device = Device,
       baud = Baud,
       reopen_timer = Reopen_Timer,
       reopen_ival = Reopen_Ival
      }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({subscribe,Pid}, _From, State) when is_pid(Pid) ->
    Mon = erlang:monitor(process, Pid),
    S = #subscription { pid=Pid, mon=Mon },
    SList = [S | State#state.sub_list],
    State1 = State#state { sub_list = SList },
    if State#state.uart =/= undefined ->
	    S#subscription.pid ! {nfc,S#subscription.mon,device_open};
       true ->
	    ok
    end,
    {reply, {ok,Mon}, State1};

handle_call({unsubscribe,Ref}, _From, State) ->
    case lists:keytake(Ref, #subscription.mon, State#state.sub_list) of
	false ->
	    {reply, ok, State};
	{value,S,SList} ->
	    demon(S#subscription.mon),
	    {reply, ok, State#state { sub_list = SList }}
    end;
handle_call({command,Ascii}, _From, State) ->
    try iolist_size(Ascii) of
	_N ->
	    %% testing arygon_nfc commands
	    if State#state.uart =:= undefined ->
		    {reply,{error,not_running},State};
	       true ->
		    send_command(State#state.uart, Ascii),
		    {reply,ok,State}
	    end
    catch
	error:_ ->
	    {reply,{error,badarg},State}
    end;

handle_call({execute,Cmds}, _From, State) ->
    State1 = run(Cmds, State#state { cmd_list=Cmds, array=array:new()}),
    {reply, ok, State1};
    
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
    
handle_call(_Request, _From, State) ->
    {reply, {error,bad_request}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
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

handle_info({'DOWN',Ref,process,_Pid,_Reason}, State) ->
    lager:debug("subscriber crashed reason=~p", [_Reason]),
    case lists:keytake(Ref, #subscription.mon, State#state.sub_list) of
	false ->
	    {noreply, State};
	{value,_S,SList} ->
	    {noreply, State#state { sub_list = SList }}
    end;    
handle_info({timeout,Ref,open_device}, State) 
  when Ref =:= State#state.reopen_timer ->
    try uart:open(State#state.device, [{baud,State#state.baud},{active,true},
				       {packet,line},
				       {csize,8},{parity,none},{stopb,1}]) of
	{ok,U} ->
	    lager:notice("uart device ~s ready", [State#state.device]),
	    notify(device_open,State),
	    {noreply, State#state { uart=U, reopen_timer=undefined }};

	Error ->
	    %% FIXME: in the case of enoent then we could use fnotify
	    %% to notify when the device is created ! (at least on mac os x)
	    lager:error("unable to open device ~s: ~p",
			[State#state.device,Error]),
	    Reopen_Timer = erlang:start_timer(State#state.reopen_ival,
					      self(), open_device),
	    {noreply, State#state { reopen_timer = Reopen_Timer }}
    catch
	error:Reason ->
	    lager:error("unable to open device ~s: ~p",
			[State#state.device,Reason]),
	    Reopen_Timer = erlang:start_timer(State#state.reopen_ival,
					      self(), open_device),
	    {noreply, State#state { reopen_timer = Reopen_Timer }}
    end;
handle_info({uart,Port,Line}, State) when Port =:= State#state.uart ->
    lager:debug("handle_info uart data ~p", [Line]),
    Line1 = strip_nl(Line),
    {noreply, process_line(Line1, State)};
handle_info({uart_error,Port,Reason}, State) when Port =:= State#state.uart ->
    if Reason =:= enxio ->
	    lager:error("uart error ~p device ~s unplugged?", 
			[Reason,State#state.device]);
       true ->
	    lager:error("uart error ~p for device ~s", 
			[Reason,State#state.device])
    end,
    {noreply, State};
handle_info({uart_closed,Port}, State) when Port =:= State#state.uart ->
    uart:close(Port),
    lager:error("uart close device ~s will retry", [State#state.device]),
    Reopen_Timer = erlang:start_timer(State#state.reopen_ival,
				      self(), open_device),
    notify(device_closed,State),
    {noreply, State#state { uart=undefined,reopen_timer = Reopen_Timer }};
handle_info(_Info, State) ->
    lager:debug("handle_info got ~p", [_Info]),
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
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

strip_nl([]) -> [];
strip_nl([$\n]) ->  [];
strip_nl([$\r,$\n]) ->  [];
strip_nl([C|Cs]) -> [C|strip_nl(Cs)].


process_line("FF000000", State) ->
    State;
process_line("FF0000"++[L1,L0|HexBinary], State) ->
    Len = list_to_integer([L1,L0], 16),
    Len0 = length(HexBinary),
    if Len0 =:= Len ->
	    case HexBinary of
		[$4,$B,A1,A0,B1,B0,R3,R2,R1,R0,S1,S0,N1,N0 | CardID] ->
		    PossibleTargets = (A1-$0)*16 + (A0-$0),
		    Target = (B1-$0)*16 + (B0-$0),
		    SensRes = [R3,R2,R1,R0],
		    SelRes  = [S1,S0],
		    IDLen = (N1-$0)*16 + (N0-$0),
		    select(PossibleTargets,Target,SensRes,SelRes,IDLen,CardID,
			   State);

		[$4,$1,$0,$1] -> %€ error code
		    run_done(error, State);

		[$4,$1,$0,$0 | Data] ->
		    io:format("Result data: [~s]\n", [Data]),
		    run_result(State#state.cmd_list, Data, State);
		_ ->
		    lager:warning("unknown data: [~s]", [HexBinary]),
		    State
	    end;
       true ->
	    lager:warning("length mismatch len=~p, format ~p",[Len,HexBinary]),
	    State
    end;
process_line(Data, State) ->
    lager:warning("Unknown data: [~s]", [Data]),
    State.

notify(Message, State) ->
    lists:foreach(
      fun(S) ->
	      S#subscription.pid ! {nfc,S#subscription.mon,Message}
      end, State#state.sub_list),
    State.

select(NTargets,Target,SendRes,SelRes,IDLen,CardID,State) ->
    lager:notice("card: ~s", [CardID]),
    lager:info("initialize targets: ~w, target=~w, sens=~s, sel=~s, len=~w",
	  [NTargets, Target, SendRes, SelRes, IDLen]),
    notify({select,CardID}, State#state { card = CardID }).


demon(Ref) ->
    erlang:demonitor(Ref),
    receive
	{'DOWN',Ref,process,_Pid,_Reason} ->
	    ok
    after 0 ->
	    ok
    end.

run(Cmds=[Cmd|_],State) ->
    lager:debug("execute cmd: ~p", [Cmd]),
    run_(Cmds,State);
run([], State) ->
    run_([],State).

run_(Cmds0=[{load,I}|Cmds1], State) when ?is_page(I) ->
    case array:get(I, State#state.array) of
	undefined ->
	    Page = tl(integer_to_list(16#100+I, 16)),
	    send_command(State#state.uart, "0r"++Page),
	    State#state { cmd_list=Cmds0 };
	_Cached ->
	    run(Cmds1, State#state { cmd_list=Cmds1 })
    end;
run_([{set,I,Y}|Cmds],State) when ?is_page(I), is_integer(Y) ->
    A = array:set(I, Y, State#state.array),
    lager:debug("A[~w] = ~8.16.0B", [I,Y]),
    run(Cmds, State#state { array = A });

run_([{copy,I,J}|Cmds],State) when ?is_page(I), ?is_page(J) ->
    Y = array:get(I, State#state.array),    
    A = array:set(J, Y, State#state.array),
    lager:debug("A[~w] = ~8.16.0B", [J,Y]),
    run(Cmds, State#state { array = A });
run_([{ite,Test,Then,Else}|Cmds],State) ->
    case eval(Test,State) of
	true ->
	    run(Then ++ Cmds, State);
	false ->
	    run(Else ++ Cmds, State)
    end;
run_(Cmds=[{save,I}|_], State) when ?is_page(I) ->
    Page = tl(integer_to_list(16#100+I, 16)),
    V = array:get(I, State#state.array) band 16#ffffffff,
    Value = tl(integer_to_list(16#100000000+V, 16)),
    send_command(State#state.uart, "0w4"++Page++Value),
    State#state { cmd_list=Cmds };
run_([{update,I,Value}|Cmds], State) when ?is_page(I), is_integer(Value) ->
    Y0 = array:get(I, State#state.array),
    Y = (Y0+Value) band 16#ffffffff,
    A = array:set(I, Y, State#state.array),
    lager:debug("A[~w] = ~8.16.0B", [I,Y]),
    run(Cmds, State#state { array = A });
run_([ok | _Cmd],State) ->
    run_done(ok, State);
run_([error | _Cmd],State) ->
    run_done(error, State);
run_([], State) ->
    run_done(ok,State).

run_result([{load,I}|Cmds], Data, State) ->
    %% this should be the result of I,I+1,I+2,I+3
    D = list_to_integer(Data, 16),
    <<X0:32,X1:32,X2:32,X3:32>> = <<D:128>>,
    A0 = State#state.array,
    A1 = array:set(I, X0, A0),
    A2 = array:set(I+1, X1, A1),
    A3 = array:set(I+2, X2, A2),
    A4 = array:set(I+3, X3, A3),
    lager:debug("A[~w] = ~8.16.0B", [I,X0]),
    lager:debug("A[~w] = ~8.16.0B", [I+1,X1]),
    lager:debug("A[~w] = ~8.16.0B", [I+2,X2]),
    lager:debug("A[~w] = ~8.16.0B", [I+3,X3]),
    run(Cmds, State#state { cmd_list=Cmds, array=A4 });
run_result([{save,_I}|Cmds], [], State) ->
    run(Cmds, State#state { cmd_list=Cmds });
run_result([], _, State) ->
    run_done(ok, State);
run_result([ok|_], _, State) ->
    run_done(ok, State);
run_result([error|_], _, State) ->
    run_done(error, State).

run_done(Status, State) ->
    notify({Status,State#state.card}, 
	   State#state { cmd_list=[], array=undefined }).


eval({gt,I,Value}, State) when ?is_page(I), is_integer(Value) ->
    array:get(I, State#state.array) > Value;
eval({gte,I,Value}, State) when ?is_page(I), is_integer(Value) ->
    array:get(I, State#state.array) >= Value;
eval({lt,I,Value}, State) when ?is_page(I), is_integer(Value) ->
    array:get(I, State#state.array) < Value;
eval({lte,I,Value}, State) when ?is_page(I), is_integer(Value) ->
    array:get(I, State#state.array) =< Value;
eval({eq,I,Value}, State) when ?is_page(I), is_integer(Value) ->
    array:get(I, State#state.array) =:= Value;
eval({neq,I,Value}, State) when ?is_page(I), is_integer(Value) ->
    array:get(I, State#state.array) =/= Value.

send_command(Uart, Command) ->
    lager:debug("uart command: ~s", [Command]),    
    uart:send(Uart, Command).

play(Status) ->
    spawn(fun() ->
		  Snd = case Status of
			    ok -> "ding.au";
			    error -> "bark.au"
			end,
		  File = filename:join(code:priv_dir(arygon_nfc),Snd),
		  os:cmd("afplay "++File)
	  end).
