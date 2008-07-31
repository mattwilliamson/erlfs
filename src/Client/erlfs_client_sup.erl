%%%-------------------------------------------------------------------
%%% File    : erlfs_client_sup.erl
%%% Author  : Matt Williamson <mwilliamson@mwvmubhhlap>
%%% Description : This is the top supervisor for the ErlFS client.
%%%
%%% Created : 31 Jul 2008 by Matt Williamson <mwilliamson@mwvmubhhlap>
%%%-------------------------------------------------------------------
-module(erlfs_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    ErlFSClient = {erlfs_client,{erlfs_client_svr, start_link, []},
	      permanent, 2000, worker, [erlfs_client_svr]},
    {ok,{{one_for_one, 0, 1}, [ErlFSClient]}}.

%%====================================================================
%% Internal functions
%%====================================================================
