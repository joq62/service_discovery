%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%%
%%% -------------------------------------------------------------------
-module(all).       
 
-export([start/0]).



-define(Appl,service_discovery).

-define(TestNodeNames,["n1","n2","n3"]).
-define(NeededList,[db,dummy]).

%%---------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
   
    ok=setup(),
    ok=config_test(),
 %   ok=send_receive_test(),
      
    io:format("Test OK !!! ~p~n",[?MODULE]),
    timer:sleep(2000),
  %  init:stop(),
    ok.
%%-----------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
send_receive_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),

    ok.
%%-----------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
config_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    ok=service_discovery:update(),
    timer:sleep(100),
    {ok,[]}=service_discovery:needed(),
    {ok,[]}=service_discovery:imported(),
    {ok,[]}=service_discovery:get_all(glurk),
    {ok,[]}=service_discovery:get_all(db),
    {ok,[]}=service_discovery:get_all(dummy),

    ok=service_discovery:config_needed(?NeededList),
    ok=service_discovery:update(),

    {ok,[db,dummy]}=service_discovery:needed(),
    {ok,ImportedList}=service_discovery:imported(),
    [{db,'n1@c50',_},{dummy,'n1@c50',_},{dummy,'n2@c50',_},{dummy,'n3@c50',_},{dummy,'test_appl@c50',_}]=lists:sort(ImportedList),
 
    {ok,[]}=service_discovery:get_all(glurk),
    {ok,[{db,'n1@c50',_}]}=service_discovery:get_all(db),
    {ok,[{dummy,'n3@c50',_},{dummy,'n2@c50',_},{dummy,'n1@c50',_},{dummy,'test_appl@c50',_}]}=service_discovery:get_all(dummy),
    
    %% kill node
    io:format("kill node ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    slave:stop('n1@c50'),
    timer:sleep(100),
    {ok,[{db,'n1@c50',_}]}=service_discovery:get_all(db),
    {ok,[{dummy,'n3@c50',_},{dummy,'n2@c50',_},{dummy,'n1@c50',_},{dummy,'test_appl@c50',_}]}=service_discovery:get_all(dummy),
    timer:sleep(11000),
    {ok,[]}=service_discovery:get_all(db),
    {ok,[{dummy,'n3@c50',_},{dummy,'n2@c50',_},{dummy,'test_appl@c50',_}]}=service_discovery:get_all(dummy),
    

    %%restart Node
    io:format("restart node ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    N1=start_node("n1"),
    true=rpc:call(N1,code,add_path,["test_ebin"],5000),
    erlang:spawn(N1,db,start,[]),
    timer:sleep(11000),
    {ok,[{db,'n1@c50',_}]}=service_discovery:get_all(db),
    {ok,[{dummy,'n3@c50',_},{dummy,'n2@c50',_},{dummy,'test_appl@c50',_}]}=service_discovery:get_all(dummy),

    %%kill process
    io:format("kill process ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    {ok,[{dummy,'n3@c50',PidToKill}|_]}=service_discovery:get_all(dummy),
    true=erlang:exit(PidToKill,kill),
    timer:sleep(11000),
    {ok,[{db,'n1@c50',_}]}=service_discovery:get_all(db),
    {ok,[{dummy,'n2@c50',_},{dummy,'test_appl@c50',_}]}=service_discovery:get_all(dummy),

    ok.

%%-----------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME,?LINE}]),
    %% 
    ConnectNode=start_node("connect"),
    true=net_kernel:connect_node(ConnectNode),
    _Nodes=[get_node(Name)||Name<-?TestNodeNames],
    Started=[start_node(Name)||Name<-?TestNodeNames],
    [true,true,true]=[rpc:call(N,net_kernel,connect_node,[ConnectNode],5000)||N<-Started],
    application:start(log),
    pong=log:ping(),  
      
    %% To be changed when create a new server
    application:start(?Appl),
    pong=?Appl:ping(),
    
    dummy:start_link(),
    timer:sleep(100),
    DummyPid=erlang:whereis(dummy),
    DummyPid=rpc:call(node(),erlang,whereis,[dummy],500),
    
    [N1,N2,N3]=get_nodes(),
    [true,true,true]=[rpc:call(N,code,add_path,["test_ebin"],5000)||N<-get_nodes()],
   
    %%
  %  R1=rpc:call(N1,dummy,start_link,[]),
  %  io:format("R1 ~p~n",[{R1,?MODULE,?FUNCTION_NAME,?LINE}]),
  %  init:stop(),
  %  timer:sleep(3000),

    R1=[{N,rpc:call(N,dummy,start,[])}||N<-get_nodes()],
    io:format("R1 ~p~n",[{R1,?MODULE,?FUNCTION_NAME,?LINE}]),
    [{N1,{ok,P1}},{N2,{ok,_P2}},{N3,{ok,_P3}}]=R1,
    _PidDb1=rpc:call(N1,db,start,[]),
    timer:sleep(1000),
  %    io:format("P1 + process_info~p~n",[{P1,rpc:call(N1,erlang,is_process_alive,[P1],5000),?MODULE,?FUNCTION_NAME,?LINE}]),
  %  P1!{timeout},
   


    ok.


start_node(Name)->
    {ok,Host}=net:gethostname(),
    Cookie=atom_to_list(erlang:get_cookie()),
    {ok,Node}=slave:start(Host,Name,"-setcookie "++Cookie),
    true=net_kernel:connect_node(Node),
    Node.

%stop_node(Name)->
%    {ok,Host}=net:gethostname(),
%    Node=list_to_atom(Name++"@"++Host),
%    slave:stop(Node).


get_nodes()->
    {ok,Host}=net:gethostname(),
    [list_to_atom(N++"@"++Host)||N<-?TestNodeNames].

get_node(Name)->
    {ok,Host}=net:gethostname(),
    list_to_atom(Name++"@"++Host).
