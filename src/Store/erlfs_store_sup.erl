%%%-------------------------------------------------------------------
%%% File    : erlfs_store_sup.erl
%%% Author  : Matt Williamson <mwilliamson@mwvmubhhlap>
%%% Description : This is the top supervisor for the ErlFS storage
%%% application.
%%%
%%% Created : 31 Jul 2008 by Matt Williamson <mwilliamson@mwvmubhhlap>
%%%-------------------------------------------------------------------
-module(erlfs_store_sup).

-behaviour(supervisor).

-import(supervisor).

%% API
-export([start_link/1]).

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
start_link(StartArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, StartArgs).

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
    ErlFSStore = {erlfs_store, {erlfs_store_svr, start_link, []},
	      permanent, 2000, worker, [erlfs_store_svr]},
    ErlFSWorkers = {store_worker_sup, {store_worker_sup, 
					     start_link, []},
	      permanent, 2000, supervisor, [erlfs_store_worker_sup]},
    {ok,{{one_for_all, 0, 1}, [ErlFSStore, ErlFSWorkers]}}.

%%====================================================================
%% Internal functions
%%====================================================================
