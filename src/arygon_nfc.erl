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
-export([start_link/0]).
-export([start_link/1]).

-export([subscribe/0, unsubscribe/1, setopts/1, getopts/1, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% "/dev/tty.SLAB_USBtoUART"

-define(SERVER, ?MODULE). 

-record(subscription,
	{
	  pid :: pid(),
	  mon :: reference()
	}).

-record(state,
	{
	  device :: string(),
	  sl :: port(),
	  baud :: non_neg_integer(),
	  reopen_timer :: undefined | reference(),
	  reopen_ival :: timeout(),
	  icard_delay_timer :: undefined | reference(),
	  icard_delay :: timeout(),
	  buf = <<>> :: binary(),
	  sub_list=[] :: [#subscription{}]
	}).

%%%===================================================================
%%% API
%%%===================================================================
subscribe() ->
    gen_server:call(?SERVER, {subscribe, self()}).

unsubscribe(Ref) ->
    gen_server:call(?SERVER, {unsubscribe, Ref}).

setopts(Opts) ->
    gen_server:call(?SERVER, {setopts,Opts}).

getopts(Opts) ->
    gen_server:call(?SERVER, {getopts,Opts}).

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
    start_link([]).

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
    Device = proplists:get_value(device, Args, "/dev/tty.usbserial"),
    Baud = proplists:get_value(baud, Args, 9600),
    Reopen_Timer = erlang:start_timer(100, self(), open_device),
    Reopen_Ival = proplists:get_value(reopen_ival, Args, 20*1000),
    ICard_Delay = proplists:get_value(icard_delay, Args, 500),
    {ok,
     #state{
       device = Device,
       baud = Baud,
       reopen_timer = Reopen_Timer,
       reopen_ival = Reopen_Ival,
       icard_delay = ICard_Delay 
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
    {reply, {ok,Mon}, State#state { sub_list = SList }};

handle_call({unsubscribe,Ref}, _From, State) ->
    case lists:keytake(Ref, #subscription.mon, State#state.sub_list) of
	false ->
	    {reply, ok, State};
	{value,S,SList} ->
	    demon(S#subscription.mon),
	    {reply, ok, State#state { sub_list = SList }}
    end;

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
    io:format("subscriber crashed reason=~p\n", [_Reason]),
    case lists:keytake(Ref, #subscription.mon, State#state.sub_list) of
	false ->
	    {noreply, State};
	{value,_S,SList} ->
	    {noreply, State#state { sub_list = SList }}
    end;    
handle_info({timeout,Ref,select_icard}, State) 
  when Ref =:= State#state.icard_delay_timer ->
    if is_port(State#state.sl) ->
	    sl:send(State#state.sl, "0s"),
	    {noreply, State#state { icard_delay_timer = undefined }};
       true ->
	    {noreply, State}
    end;
handle_info({timeout,Ref,open_device}, State) 
  when Ref =:= State#state.reopen_timer ->
    try sl:open(State#state.device, [{baud,State#state.baud},
				     {csize,8},{parity,0},{stopb,1}]) of
	{ok,S} ->
	    sl:send(S, "0s"),  %% ready for a reading
	    {noreply, State#state { sl = S, reopen_timer=undefined }};
	Error ->
	    io:format("unable to open device ~s: ~p\n", 
		      [State#state.device,Error]),
	    Reopen_Timer = erlang:start_timer(State#state.reopen_ival,
					      self(), open_device),
	    {noreply, State#state { reopen_timer = Reopen_Timer }}
    catch
	error:Reason ->
	    io:format("unable to open device ~s: ~p\n", 
		      [State#state.device,Reason]),
	    Reopen_Timer = erlang:start_timer(State#state.reopen_ival,
					      self(), open_device),
	    {noreply, State#state { reopen_timer = Reopen_Timer }}
    end;

handle_info({Port,{data,D}}, State) when Port =:= State#state.sl ->
    case binary:split(<<(State#state.buf)/binary, D/binary>>, <<"\r\n">>) of
	[Buf] ->
	    {noreply,State#state { buf = Buf}};
	[Line,Buf] ->
	    State1 = process_line(Line, State),
	    {noreply,State1#state { buf = Buf}}
    end;

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

process_line(<<"FF0000",L1,L0,HexBinary/binary>>, State) ->
    Len = list_to_integer([L1,L0], 16),
    if byte_size(HexBinary) =:= Len ->
	    if Len =:= 0 ->
		    %% response code for 0s
		    State;
	       Len =:= 28 ->  %% NFC code
		    State1 = icard_delay(State),
		    notify(HexBinary, State1);
	       true ->
		    io:format("unknown format ~p\n",[HexBinary]),
		    icard_delay(State)
	    end;
       true ->
	    io:format("length mismatch len=~p, format ~p\n",[Len,HexBinary]),
	    icard_delay(State)
    end.

notify(HexBinary, State) ->
    lists:foreach(
      fun(S) ->
	      S#subscription.pid ! {nfc,self(),HexBinary}
      end, State#state.sub_list),
    State.

icard_delay(State) ->
    if is_reference(State#state.icard_delay_timer) ->
	    State;
       true ->
	    Timer = erlang:start_timer(State#state.icard_delay,
				       self(), select_icard),
	    State#state { icard_delay_timer = Timer }
    end.
	    

demon(Ref) ->
    erlang:demonitor(Ref),
    receive
	{'DOWN',Ref,process,_Pid,_Reason} ->
	    ok
    after 0 ->
	    ok
    end.
