%%%-------------------------------------------------------------------
%% @doc application_server public API
%% @end
%%%-------------------------------------------------------------------

-module(service_discovery_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    service_discovery_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
