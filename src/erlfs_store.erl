%%% @author Matt Williamson <mwilliamson@dawsdesign.com>
%%% @doc This application is used to store and retrieve file chunks in 
%%% an ErlFS cluster.
%%% @headerfile "../include/erlfs.hrl"
-module(erlfs_store).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([chunk_store/2, chunk_get/2]).

-define(SERVER, erlfs_store_svr).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec chunk_store(Node, Chunk) -> file_id() | {error, Reason}
%%     Node = node()
%% @doc Store a file chunk on a specified node. 
%% Used by {@link erlfs_client}.
%% The caller must recover if the specified node is down.
%%--------------------------------------------------------------------
chunk_store(Node, Chunk) ->
    gen_server:call({?SERVER, Node}, {store_chunk, Chunk}).

%%--------------------------------------------------------------------
%% @spec chunk_get(Node, ChunkID) -> ok | {error, Reason}
%%     Node = node()
%%     ChunkID = chunk_id()
%% @doc Retrieve a file chunk from a specified node. 
%% Used by {@link erlfs_client}.
%% The caller must recover if the specified node is down.
%%--------------------------------------------------------------------
chunk_get(Node, ChunkID) ->
    Ref = make_ref(),
    gen_server:call({?SERVER, Node}, {get_chunk, Ref, ChunkID}).


%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, StartArgs) ->
    case erlfs_store_sup:start_link(StartArgs) of
	{ok, Pid} -> 
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
