%%%-------------------------------------------------------------------
%%% File    : erlfs_store_worker_sup.erl
%%% Author  : Matt Williamson <mwilliamson@mwvmubhhlap>
%%% Description : This is a simple_one_for_one supervisor where worker
%%% processes are spawned to save and replicate file chunks.
%%%
%%% Created :  1 Aug 2008 by Matt Williamson <mwilliamson@mwvmubhhlap>
%%%-------------------------------------------------------------------
-module(erlfs_store_worker_sup).

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
init(Chunk) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{erlfs_store_worker_fsm, 
	    {erlfs_store_worker_fsm, start_link, Chunk},
            temporary, brutal_kill, worker, 
	    [erlfs_store_worker_fsm]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================
