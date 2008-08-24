%%% @author Matt Williamson <mwilliamson@dawsdesign.com>
%%% @doc This application is used to store and retrieve file chunks in 
%%% an ErlFS cluster. When a client wants to retrieve a chunk of a
%%% file, it calls chunk_get/2. If it wants to store a chunk, it
%%% calls chunk_store/2.
%%% @headerfile "../include/erlfs.hrl"
-module(erlfs_store).

-include("erlfs.hrl").
-include_lib("eunit.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API
-export([store_chunk/2, get_chunk/2]).

-define(SERVER, erlfs_store_svr).

%%====================================================================
%% API
%%====================================================================
%% @spec store_chunk(Node, Chunk) -> file_id() | {error, Reason}
%%     Node = node()
%%
%% @doc Store a file chunk on a specified node. 
%% Used by {@link erlfs_client}.
%% The caller must recover if the specified node is down.
%%
%% @end
%%--------------------------------------------------------------------
store_chunk(Node, Chunk) ->
    storing_chunk = gen_server:call({?SERVER, Node}, {store_chunk, Chunk}),
    Timeout = get_timeout(),
    Status = receive 
		 {store_status, ok} -> ok;
		 Error -> Error
	     after Timeout -> {error, timeout}
	     end,
    Status.


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
    %% @todo Change this to pass around chunk records
    gen_server:call({?SERVER, Node}, {get_chunk, ChunkID}),
    Timeout = get_timeout(),
    Status = receive 
		 {get_chunk, ok, ChunkData} -> 
		     {ok, ChunkData};
		 Error -> Error
	     after Timeout -> {error, timeout}
	     end,
    Status.

%%====================================================================
%% Tests
%%====================================================================
%%--------------------------------------------------------------------
%% @private
%%
%% @spec store_chunk_test() -> ok
%%
%% @doc Test storing a chunk
%%
%% @end
%%--------------------------------------------------------------------
store_chunk_test() ->
    Data = <<"Hello world!">>,
    FileID = "5eb63bbbe01eeed093cb22bb8f5acdc3",
    FileMeta = #file_meta{id=FileID, name="test.txt", type="text/plain"},
    ChunkMeta = #chunk_meta{file_meta=FileMeta},
    Chunk = #chunk{chunk_meta=ChunkMeta, data=Data},
    ok = store_chunk(node(), Chunk).

%%--------------------------------------------------------------------
%% @private
%%
%% @spec get_chunk_test() -> ok
%%
%% @doc Test retrieving a chunk.
%%
%% @end
%%--------------------------------------------------------------------
get_chunk_test() ->
    FileID = "5eb63bbbe01eeed093cb22bb8f5acdc3",
    ChunkNumber = 0,
    ChunkID = {FileID, ChunkNumber},
    {ok, ChunkData} = get_chunk(node(), ChunkID),
    <<"Hello world!">> = ChunkData.


%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @private
%%
%% @spec start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%%     Pid = pid()
%%
%% @doc This function is called whenever an application 
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
%% @doc This function is called whenever an application
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
get_timeout() ->
    case application:get_env(erlfs_store, transfer_timeout) of
	{ok, Value} -> 
	    Value;
	undefined ->
	    15000
    end.
