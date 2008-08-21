%%%-------------------------------------------------------------------
%%% @private
%%%
%%% @author Matt Williamson <mwilliamson@dawsdesign.com>
%%%
%%% @doc This is a library module for erlfs_store_svr to
%%% perform common tasks.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlfs_store_lib).

-include("erlfs.hrl").

%% API
-export([store_chunk/1, get_chunk/1]).

%%====================================================================
%% API
%%====================================================================
%%-------------------------------------------------------------------
%% @spec store_file_chunk(Chunk) -> ok | {error, Reason}
%%
%% @doc Store a piece of a file on the filesystem.
%%
%% The path where it is stored is a) the folder path hashed and b)
%% the file path + file name hashed. The hashes are split into two
%% byte chunks, e.g. /ab/cd/ef/12/... This prevents too many files or
%% directories in on directory, as is limited by certain filesystems.
%% The chunk will be stored under the filename n, where n is the chunk
%% number.
%%
%% @end
%%--------------------------------------------------------------------
store_chunk(Chunk) ->
    ChunkMeta = Chunk#chunk.chunk_meta,
    Data = Chunk#chunk.data,
    FileMeta = ChunkMeta#chunk_meta.file_meta,
    ChunkID = {FileMeta#file_meta.id, ChunkMeta#chunk_meta.number},
    FinalPath = chunk_id_to_path(ChunkID),
    %% Create any folders necessary
    ok = filelib:ensure_dir(FinalPath),
    file:write_file(FinalPath, Data).


get_chunk(ChunkID) ->
    FinalPath = chunk_id_to_path(ChunkID),
    {ok, Data} = file:read_file(FinalPath),
    Data.


%%====================================================================
%% Internal Functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec hash_to_path(HashBinary) -> HashPath
%%     HashBinary = binary()
%%     HashPath = string()
%%
%% @doc Converts hash e.g. abcdef... to directory path e.g. 
%% /ab/cd/ef/12/34/56....
%% This is the method to spread files across many directories to avoid
%% OS errors due to either path too long or too many files/directories.
%%
%% @end
%%--------------------------------------------------------------------
hash_to_path(HashBinary) when is_binary(HashBinary) ->
    hash_to_path(binary_to_list(HashBinary), []);
hash_to_path(HashString) when is_list(HashString) ->
    hash_to_path(HashString, []).
hash_to_path([], Path) ->
    Path;
hash_to_path([A,B|Rest], NewList) ->
    hash_to_path(Rest, [A, B, "/"] ++ NewList).

%%--------------------------------------------------------------------
%% @spec chunk_to_path(ChunkMeta) -> Path
%%     Path = string()
%%
%% @doc Determine the filesystem location of a file chunk. Returns 
%% something like /ab/cd/ef/12/34/56/78/90/AB/.../0
%%
%% @end
%%--------------------------------------------------------------------
chunk_id_to_path({FileID, ChunkNumber}) ->
    {ok, DataDir} = application:get_env(erlfs_store, data_dir),
    NumberString = integer_to_list(ChunkNumber),
    filename:join([DataDir, hash_to_path(FileID), NumberString]).
