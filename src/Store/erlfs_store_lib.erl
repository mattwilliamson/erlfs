%%%-------------------------------------------------------------------
%%% File    : erlfs_store_lib.erl
%%% Author  : Matt Williamson <mwilliamson@mwvmubhhlap>
%%% Description : This is a library module for erlfs_store_svr to
%%% perform common tasks.
%%%
%%% Created : 24 Jul 2008 by Matt Williamson <mwilliamson@mwvmubhhlap>
%%%-------------------------------------------------------------------
-module(erlfs_store_lib).

-include("erlfs.hrl").

%% API
-export([store_file_chunk/1]).

%%====================================================================
%% API
%%====================================================================
%%-------------------------------------------------------------------
%% Function: store_file_chunk
%% Description: Store a piece of a file on the filesystem.
%%
%% The path where it is stored is a) the folder path hashed and b)
%% the file path + file name hashed. The hashes are split into two
%% byte chunks, e.g. /ab/cd/ef/12/... This prevents too many files or
%% directories in on directory, as is limited by certain filesystems.
%% The chunk will be stored under the filename n, where n is the chunk
%% number.
%%--------------------------------------------------------------------
store_file_chunk(#chunk{chunk_meta=#chunk_meta{
			  file_meta=#file_meta{
			    full_path=FullPath, 
			    path=Path, 
			    name=_Name, 
			    size=Size, 
			    created={_CreatedDate, _CreatedTime}}, 
			  number=Number, 
			  size=Size, 
			  nodes=_Nodes}, 
			data=Data}) ->
    DirPathHash = crypto:sha(Path),
    FilePathHash = crypto:sha(FullPath),
    FinalPath = hash_to_path(DirPathHash) ++ hash_to_path(FilePathHash),
    ok = file_lib:ensure_dir(FinalPath), % make sure entire path exists
    File = file:open(?DATA_DIR ++ "/" ++ FinalPath ++ "." ++ Number, 
		     [write, raw]),
    Status = file:write(File, Data),
    file:close(File),
    Status.


%%====================================================================
%% Internal Functions
%%====================================================================
%%-------------------------------------------------------------------

%% Converts hash e.g. abcdef... to directory path e.g. /ab/cd/ef..
hash_to_path(HashBinary) when is_binary(HashBinary) ->
    hash_to_path(binary_to_list(HashBinary), []);
hash_to_path(HashString) when is_list(HashString) ->
    hash_to_path(HashString).
hash_to_path([A,B|Rest], NewList) ->
    hash_to_path(Rest, [A ++ B ++ "/" | NewList]).
