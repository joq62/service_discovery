%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%% 
%%% @end
%%% Created : 18 Apr 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(service_discovery). 
 
-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include 
%%
%%--------------------------------------------------------------------

-include("log.api").

-define(LoopInterval,10*1000).

%% To be changed when create a new server
%-include("service_discovery.hrl").

%% API

-export([
	 config_needed/1,
	 config_exported/1,
	 update/0,
	 update_loop/0,

	 get_all/1,
%	 get_local_host/1,
	 
	 needed/0,
	 imported/0,

	 update_reply/1
	 
	]).


%% admin




-export([
	 start/0,
	 ping/0,
	 stop/0
	]).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
		     
-record(state, {
		needed,
		imported,
		exported
	        
	       }).

%%%===================================================================
%%% API
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
start()->
    application:start(?MODULE).

%%--------------------------------------------------------------------
%% @doc
%% Used to check if the application has started correct
%% @end
%%--------------------------------------------------------------------
-spec config_needed(NeededList::term()) -> ok.
config_needed(NeededList)-> 
    gen_server:cast(?SERVER, {config_needed,NeededList}).

%%--------------------------------------------------------------------
%% @doc
%% Used to check if the application has started correct
%% @end
%%--------------------------------------------------------------------
-spec config_exported(ServiceId::term()) -> ok.
config_exported(ServiceId)-> 
    gen_server:cast(?SERVER, {config_exported,ServiceId}).

%%--------------------------------------------------------------------
%% @doc
%% Used to check if the application has started correct
%% @end
%%--------------------------------------------------------------------
-spec needed() -> {ok,NeededList::term}.
needed()-> 
    gen_server:call(?SERVER, {needed},infinity).
%%--------------------------------------------------------------------
%% @doc
%% Used to check if the application has started correct
%% @end
%%--------------------------------------------------------------------
-spec imported() -> {ok,ImportedList::term}.
imported()-> 
    gen_server:call(?SERVER, {imported},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Used to check if the application has started correct
%% @end
%%--------------------------------------------------------------------
-spec get_all(Tag::atom()) -> {ok,ServiceIdList::term()}.
get_all(Tag)-> 
    gen_server:call(?SERVER, {get_all,Tag},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Used to check if the application has started correct
%% @end
%%--------------------------------------------------------------------
-spec ping() -> pong | Error::term().
ping()-> 
    gen_server:call(?SERVER, {ping},infinity).

%%--------------------------------------------------------------------
%% @doc
%% Used to check if the application has started correct
%% @end
%%--------------------------------------------------------------------
-spec update() -> ok.
update()-> 
    gen_server:cast(?SERVER, {update}).

%%--------------------------------------------------------------------
%% @doc
%% Used to check if the application has started correct
%% @end
%%--------------------------------------------------------------------
-spec update_loop() -> ok.
update_loop()-> 
    gen_server:cast(?SERVER, {update_loop}).
%%--------------------------------------------------------------------
%% @doc
%% Used to check if the application has started correct
%% @end
%%--------------------------------------------------------------------
-spec update_reply(ImportedList::term()) -> ok.
update_reply(ImportedList)-> 
    gen_server:cast(?SERVER, {update_reply,ImportedList}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%stop()-> gen_server:cast(?SERVER, {stop}).
stop()-> gen_server:stop(?SERVER).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.

init([]) ->
    
    spawn(fun()->loop() end),
    {ok, #state{
	    needed=[],
	    imported=[],
	    exported=[]
	    
	   }}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.



handle_call({get_all,WantedTag}, _From, State) ->
    Reply={ok,[{Tag,Node,Pid}||{Tag,Node,Pid}<-State#state.imported,
			       Tag=:=WantedTag]},
    {reply, Reply, State};

handle_call({needed}, _From, State) ->
    Reply={ok,State#state.needed},
    {reply, Reply, State};

handle_call({imported}, _From, State) ->
    Reply={ok,State#state.imported},
    {reply, Reply, State};

%%----- Admin ---------------------------------------------------------------

handle_call({ping}, _From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(UnMatchedSignal, From, State) ->
   ?LOG_WARNING("Unmatched signal",[UnMatchedSignal]),
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal, From,?MODULE,?LINE}]),
    Reply = {error,[unmatched_signal,UnMatchedSignal, From]},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------

handle_cast({config_needed,NeededList}, State) ->
    {ok,ImportedList}=lib_service_discovery:update(NeededList),
    NewState=State#state{needed=NeededList,
			 imported=ImportedList},
    {noreply,NewState};

handle_cast({config_exported,ExportedList}, State) ->
    NewState=State#state{exported=ExportedList},
    {noreply,NewState};

handle_cast({update}, State) ->
    {ok,ImportedList}=lib_service_discovery:update(State#state.needed),
    NewImportedList=lists:sort(ImportedList),
    NewState=State#state{imported=NewImportedList},
    {noreply,NewState};


handle_cast({update_loop}, State) ->
    {ok,ImportedList}=lib_service_discovery:update(State#state.needed),
    NewImportedList=lists:sort(ImportedList),
    NewState=if
		 State#state.imported=:=NewImportedList->
		     State;
		 true->
		     AddedApplications=[ServiceId||ServiceId<-NewImportedList,
						   false=:=lists:member(ServiceId,State#state.imported)],
		     RemovedApplications=[ServiceId||ServiceId<-State#state.imported,
						     false=:=lists:member(ServiceId,NewImportedList)],
		     if
			 AddedApplications=/=[]->
			     ?LOG_NOTICE("Application added ",[AddedApplications]);
			 true->
			     ok
		     end,
		     if
			 RemovedApplications=/=[]->
			     ?LOG_NOTICE("Application removed ",[RemovedApplications]);
			 true->
			     ok
		     end,?LOG_NOTICE("Uppdated imported applications ",[NewImportedList]),
		     State#state{imported=NewImportedList}
	     end,
    spawn(fun()->loop() end),
    {noreply,NewState};

handle_cast({update_reply,ImportedList}, State) ->
    NewState=State#state{imported=ImportedList},
    spawn(fun()->sd:update_reply() end),
    {noreply,NewState};

handle_cast({stop}, State) ->
    {stop,normal,ok,State};

handle_cast(UnMatchedSignal, State) ->
    ?LOG_WARNING("Unmatched signal",[UnMatchedSignal]),
    io:format("unmatched_signal ~p~n",[{UnMatchedSignal,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.
handle_info(timeout, State) ->

    ?LOG_NOTICE("Server started ",[?MODULE]),
    {noreply, State};

handle_info(Info, State) ->
    ?LOG_WARNING("Unmatched signal",[Info]),
    io:format("unmatched_signal ~p~n",[{Info,?MODULE,?LINE}]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
loop()->
    timer:sleep(?LoopInterval),
    rpc:cast(node(),?MODULE,update_loop,[]).
