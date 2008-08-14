%%%-------------------------------------------------------------------
%%% @author Matt Williamson <mwilliamson@dawsdesign.com>
%%%
%%% @doc This application is used to store and retrieve file chunks in 
%%% an ErlFS cluster. When a client wants to retrieve a chunk of a
%%% file, it calls {@link chunk_get/2}. If it wants to store a chunk, it
%%% calls {@link chunk_store/2}.
%%% 
%%% @headerfile "../include/erlfs.hrl"
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlfs_store).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([store_chunk/2, get_chunk/2]).

-define(SERVER, erlfs_store_svr).

%%====================================================================
%% API
%%====================================================================
%% @spec store_chunk(Node, Chunk) -> {ok, chunk_stored} | {error, Reason}
%%     Node = node()
%% 
%% @doc Store a file chunk on a specified node. 
%% Used by {@link erlfs_client}.
%% The caller must try another node if the specified node is down.
%% 
%% @end
%%--------------------------------------------------------------------

store_chunk(Node, Chunk) ->
    gen_server:call({?SERVER, Node}, {store_chunk, Chunk}).


%%--------------------------------------------------------------------
%% @spec get_chunk(Node, ChunkID) -> ok | {error, Reason}
%%     Node = node()
%%     ChunkID = chunk_id()
%% 
%% @doc Retrieve a file chunk from a specified node. 
%% Used by {@link erlfs_client}.
%% The caller must try a different node if the specified node is down.
%% 
%% @end
%%--------------------------------------------------------------------

get_chunk(Node, ChunkID) ->
    Ref = make_ref(),
    gen_server:call({?SERVER, Node}, {get_chunk, Ref, ChunkID}).


%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @private
%%
%% @spec start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Pid = pid()
%% 
%% @doc ** Application Callback **
%%
%% This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% 
%% @end
%%--------------------------------------------------------------------

start(_Type, StartArgs) ->
    case erlfs_store_sup:start_link(StartArgs) of
	{ok, Pid} -> 
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% @private
%%
%% @spec stop(State) -> void()
%% 
%% @doc ** Application Callback **
%% 
%% This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%% 
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
