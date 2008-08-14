%%%====================================================================
%%% Erlfs main include file.
%%%====================================================================

%% Types
%% @type file_id() = string(). Special ID returned when calling 
%% {@link erlfs_client:file_store/2}.
%% @type chunk_number() = integer(). The nth chunk of a file. 0-based.
%% @type chunk_id() = {file_id(), chunk_number()}.

%% Macros
-define('DATA_DIR', "/home/mwilliamson/erlfs/data").
-define('RPC_TIMEOUT', 2000).
-define('CHUNK_TRANSFER_TIMEOUT', 15000).
-define('CHUNK_MAX_SIZE', 4194304). %% 4MB

%% Mnesia Records
-record(file_meta, {full_path="", path="", name="", size=0, 
		    created={{0, 1, 1}, {0, 0, 0}}}).
-record(file, {file_meta=#file_meta{}, data}).

-record(chunk_meta, {file_meta=#file_meta{}, number=0, size=0, nodes=[]}).
-record(chunk, {chunk_meta=#chunk_meta{}, data}).

-record(directory, {path, subdirectories=[]}).
