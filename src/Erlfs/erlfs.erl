%%%-------------------------------------------------------------------
%%% File    : erlfs.erl
%%% Author  : Matt Williamson <mwilliamson@mwvmubhhlap>
%%% Description : Erlfs common functions. These are used across
%%% multiple servers (i.e. Client, Tracker and Store)
%%%
%%% Created : 31 Jul 2008 by Matt Williamson <mwilliamson@mwvmubhhlap>
%%%-------------------------------------------------------------------
-module(erlfs).

-include("erlfs.hrl").

%% API
-export([whereis_registered/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: whereis_name(Name)
%% Description: Gets a list of nodes that the calling process is
%% connected to has a process is registered as Name
%%--------------------------------------------------------------------

%% Old version. Looks expensive, not based on benchmarks.
%% whereis_registered(PName) ->
%%     Nodes = nodes(),
%%     Caller = self(),
%%     Test = fun() -> 
%% 		   HasName = lists:any(
%% 			       fun(H) -> H == PName end, 
%% 			       registered()),
%% 		   Caller ! {hasname, node(), HasName}
%% 	   end,
%%     lists:foreach(fun(Node) ->
%% 			  spawn(Node, Test) 
%% 		  end, 
%% 		  Nodes),
%%     whereis_registered_collect(Nodes, []).

whereis_registered(PName) ->
    Nodes = nodes(),
    Request = test,
    {Replies, _BadNodes} = gen_server:multi_call(Nodes, PName, Request),
    [Node || {Node, ok}  <- Replies].

%%====================================================================
%% Internal functions
%%====================================================================

%% whereis_registered_collect([], Nodes) ->
%%     Nodes;
%% whereis_registered_collect([_|Waiting], Nodes) ->
%%     % We are just using the original node list for counting the number
%%     % of results we get back.
%%     receive {hasname, Node, Response} ->
%% 	    case Response of
%% 		true -> whereis_registered_collect(Waiting, [Node|Nodes]);
%% 		false -> whereis_registered_collect(Waiting, Nodes)
%% 	    end
%%     after ?RPC_TIMEOUT ->
%% 	    Nodes
%%     end.
