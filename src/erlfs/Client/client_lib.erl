%%%-------------------------------------------------------------------
%%% File    : erlfs_client_lib.erl
%%% Author  : Matt Williamson <mwilliamson@mwvmubhhlap>
%%% Description : This module contains functions for clients to inter-
%%% act with an ErlFS cluster.
%%%
%%% Created : 24 Jul 2008 by Matt Williamson <mwilliamson@mwvmubhhlap>
%%%-------------------------------------------------------------------
-module(erlfs.client_lib).

-include("erlfs.hrl").

%% API
-export([put_file/2, get_file/2]).

%%====================================================================
%% API
%%====================================================================
%%
%% Nodes = [Node]. These functions will move to the next node in the 
%% list if the current one fails to perform an operation.
%%
%%--------------------------------------------------------------------
%% Function: get_file
%% Description: Retrieve a file stored on an ErlFS cluster.
%%--------------------------------------------------------------------
get_file(#file_meta{}, _Nodes) ->
    #file{}.

%%--------------------------------------------------------------------
%% Function: put_file
%% Description: Store a file on an ErlFS cluster.
%%--------------------------------------------------------------------
put_file(#file{}, _Nodes) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
