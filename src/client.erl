%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 29 Dec 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(client).

%% API
-export([
	 server_pid/1,
	 call/3,
	 cast/2
	]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
server_pid(ApplicationId)->
    case service_discovery:get_all(ApplicationId) of
	{ok,[]}->
	    {error,["undefined",ApplicationId]};
	{ok,[{ApplicationId,_Node,ServerPid}|_]} ->
	    {ok,ServerPid}
    end.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
call(ServerPid,Request,Timeout)->
    AliasReqId = alias([reply]),
    ServerPid ! {request, AliasReqId, Request},
    %% Alias will be automatically deactivated if we receive a reply
    %% since we used the 'reply' option...
    receive
        {reply, AliasReqId, Result} -> Result
    after Timeout->
            unalias(AliasReqId),
            %% Flush message queue in case the reply arrived
            %% just before the alias was deactivated...
            receive 
		{reply, AliasReqId, Result} -> Result
            after 0 -> 
		    {error,["timeout ", ServerPid,Request,Timeout]}
			%exit(timeout)
            end
    end.
%%--------------------------------------------------------------------
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
cast(ServerPid,Request)->
    AliasReqId = alias([]),
    ServerPid ! {request, AliasReqId, Request},
    %% Alias will be automatically deactivated if we receive a reply
    %% since we used the 'reply' option...
     receive
        {reply, AliasReqId, Result} -> Result
     after 0 ->
            unalias(AliasReqId),
            %% Flush message queue in case the reply arrived
            %% just before the alias was deactivated...
            receive 
		{reply, AliasReqId, Result} -> Result
            after 0 -> 
		    ok
            end
    end.



    
%%%===================================================================
%%% Internal functions
%%%===================================================================
