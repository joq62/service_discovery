%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 28 Dec 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_service_discovery).

%% API
-export([
	 update/1
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
update(NeededList)->
    {ok,Host}=net:gethostname(),
    ConnectNode=list_to_atom("connect"++"@"++Host),
    net_kernel:connect_node(ConnectNode),
    Registered=[{N,rpc:call(N,erlang,registered,[],5000)}||N<-[node()|nodes()]],
    ImportedList=update(Registered,NeededList,[]),    
    {ok,ImportedList}.
    
update([],_,Acc)->
    Acc;
update([{Node,_Registered}|T],NeededList,Acc)->
    ServiceIds=[{Tag,Node,rpc:call(Node,erlang,whereis,[Tag],5000)}||Tag<-NeededList,
								     undefined=/=rpc:call(Node,erlang,whereis,[Tag],5000)],
    
    io:format("DBG ServiceIds ~p~n",[{ServiceIds,?MODULE,?LINE}]),
    NewAcc=case ServiceIds of
	       []->
		   Acc;
	       ServiceIds->
		   lists:append(ServiceIds,Acc)
	   end,
    update(T,NeededList,NewAcc).
		       
    

%%%===================================================================
%%% Internal functions
%%%===================================================================
