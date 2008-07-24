%%% File    : erlfs.hrl
%%% Author  : Matt Williamson <mwilliamson@mwilliamson-ubuntu-vm>
%%% Description : Holds global definitions for ErlFS
%%% Created : 21 Jul 2008 by Matt Williamson <mwilliamson@mwilliamson-ubuntu-vm>

%% Macros
-define(SERVER, ?MODULE).

%% Mnesia Records
-record(file, {path, name, size, type, created, modified, accessed, data}).

