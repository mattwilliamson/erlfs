%%%-------------------------------------------------------------------
%%% @private
%%%
%%% @author Matt Williamson <mwilliamson@dawsdesign.com>
%%%
%%% @doc This is a simple_one_for_one supervisor where worker
%%% processes are spawned to save file chunks.
%%%
%%% @end
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
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% 
%% @doc Starts the supervisor.
%%
%% @end
%%--------------------------------------------------------------------
start_link() ->
    io:format("Started: ~p~n", [?MODULE]),
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%%
%% @doc Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    WorkerSpec = {erlfs_store_worker_fsm, {erlfs_store_worker_fsm, 
					   start_link, []},
		  temporary, 2000, worker, [erlfs_store_worker_fsm]},
    {ok,{{one_for_all, 0, 1}, [WorkerSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
