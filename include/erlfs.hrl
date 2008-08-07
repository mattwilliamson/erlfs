%%% File    : erlfs.hrl
%%% Author  : Matt Williamson <mwilliamson@mwilliamson-ubuntu-vm>
%%% Description : Holds global definitions for ErlFS
%%% Created : 21 Jul 2008 by Matt Williamson <mwilliamson@mwilliamson-ubuntu-vm>

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
