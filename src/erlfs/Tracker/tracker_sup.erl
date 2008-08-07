%%%-------------------------------------------------------------------
%%% File    : supervisor.erl
%%% Author  : Matt Williamson <mwilliamson@mwilliamson-ubuntu-vm>
%%% Description : This is the top supervisor. It will start and
%%% monitor the ErlFS tracker server.
%%%
%%% Created : 21 Jul 2008 by Matt Williamson <mwilliamson@mwilliamson-ubuntu-vm>
%%%-------------------------------------------------------------------
-module(erlfs.tracker_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link(StartArgs) -> {ok,Pid} | ignore | {error,Error}
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
init(StartArgs) ->
    ErlFSTracker = {erlfs.tracker_svr, 
		   {erlfs.tracker_svr, start_link, StartArgs},
		   permanent, 2000, worker, [erlfs.tracker_svr]},
    {ok, {{one_for_one, 5, 1}, [ErlFSTracker]}}.

%%====================================================================
%% Internal functions
%%====================================================================
