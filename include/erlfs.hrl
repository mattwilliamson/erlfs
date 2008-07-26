%%% File    : erlfs.hrl
%%% Author  : Matt Williamson <mwilliamson@mwilliamson-ubuntu-vm>
%%% Description : Holds global definitions for ErlFS
%%% Created : 21 Jul 2008 by Matt Williamson <mwilliamson@mwilliamson-ubuntu-vm>

%% Macros
-define('DATA_DIR', "/home/mwilliamson/erlfs/data").

%% Mnesia Records
-record(file_meta, {path, name, size, accessed, modified}).
-record(file, {file_meta=#file_meta{}, data}).
-record(file_chunk, {file_meta=#file_meta{}, chunk_number, data}).
-record(directory, {path, subfolders=[]}).
