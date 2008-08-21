%%%-------------------------------------------------------------------
%%% @author Matt Williamson <mwilliamson@mwvmubhhlap>
%%%
%%% @doc Erlfs common utility functions. These are used across
%%% multiple servers (i.e. Client, Tracker and Store)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlfs_util).

-include("erlfs.hrl").

%% API
-export([whereis_gen_server/1]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec whereis_gen_server(PName) -> Nodes
%%     Nodes = [node()]
%%
%% @doc Gets a list of nodes that the calling process is
%% connected to and has a gen_server that is registered as PName.
%% 
%% This requeires the gen_server being located to be able to respond
%% with the atom 'ok' for the message 'test'.
%%
%% @end
%%--------------------------------------------------------------------
whereis_gen_server(PName) ->
    Nodes = nodes(),
    Request = test,
    {Replies, _BadNodes} = gen_server:multi_call(Nodes, PName, Request),
    [Node || {Node, ok} <- Replies].

% Old version. Looks expensive, not based on benchmarks.
% whereis_registered(PName) ->
%     Nodes = nodes(),
%     Caller = self(),
%     Test = fun() -> 
% 		   HasName = lists:any(
% 			       fun(H) -> H == PName end, 
% 			       registered()),
% 		   Caller ! {hasname, node(), HasName}
% 	   end,
%     lists:foreach(fun(Node) ->
% 			  spawn(Node, Test) 
% 		  end, 
% 		  Nodes),
%     whereis_registered_collect(Nodes, []).

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
