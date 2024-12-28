%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2024, c50
%%% @doc
%%%
%%% @end
%%% Created : 27 Dec 2024 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(sd).

%% API
-export([
	 call/3
	 
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% call
%% @end
%%--------------------------------------------------------------------



%%--------------------------------------------------------------------
%% @doc
%% call
%% @end
%%--------------------------------------------------------------------
call(ServerPId,Masg,Timeout)->
 %   AliasReqId = alias([reply]),
 
%   ServerPid ! {request, AliasReqId, Request},
    %% Alias will be automatically deactivated if we receive a reply
    %% since we used the 'reply' option...
 %   receive
  %      {reply, AliasReqId, Result} -> Result
  
%  after 5000 ->
 %           unalias(AliasReqId),
            %% Flush message queue in case the reply arrived
            %% just before the alias was deactivated...
  %          receive {reply, AliasReqId, Result} -> Result
   %         after 0 -> exit(timeout)
    %        end
   % end,
    ok.

    





%%%===================================================================
%%% Internal functions
%%%===================================================================
