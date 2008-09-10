%%%-------------------------------------------------------------------
%%% @copyright 2008 Matt Williamson <mwilliamson@dawsdesign.com>
%%% @author Matt Williamson <mwilliamson@mwlapvm>
%%% @doc This is a plaintext logger which has the ability to rotate
%%% between log files when they are considered full. When it reaches
%%% the last file, it will open the first file.
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(rotating_logger).

-behaviour(gen_event).

-define(SERVER, ?MODULE).
-define(LINE_FORMAT, 
	"~4.4.0w-~2.2.0w-~2.2.0wT~2.2.0w:~2.2.0w:~2.2.0wZ\t~s\t~s~n").

%% API
-export([start_link/0, add_handler/0, add_handler/1, log/2, error/1,
	 warn/1, info/1, console/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-record(state, {name, 
		dir,
		max_files, 
		max_bytes, 
		current_bytes,
		current_number,
		current_fd}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok, Pid} | {error, Error} 
%% @doc Creates an event manager.
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?SERVER}). 

%%--------------------------------------------------------------------
%% @spec add_handler() -> ok | {'EXIT',Reason} | term()
%% @doc Start without any arguments.
%% @end
%%--------------------------------------------------------------------
add_handler() ->
    add_handler([]).

%%--------------------------------------------------------------------
%% @spec add_handler(PropList) -> ok | {'EXIT',Reason} | term()
%% @doc Add the event handler to error_logger. PropList is the same
%% as in {@link init/1}
%% @end
%%--------------------------------------------------------------------
add_handler(PropList) ->
    gen_event:add_handler(error_logger, ?MODULE, PropList).

%%--------------------------------------------------------------------
%% @spec log(Class, Message) -> ok
%%     Class = atom()
%%     Message = string()
%% @doc Logs an event to the error logger. This serves as a base for
%% error/1, warn/1, console/1 and info/1.
%% @end
%%--------------------------------------------------------------------
log(Class, Message) ->
    gen_event:notify(error_logger, {Class, Message}).

%%--------------------------------------------------------------------
%% @spec error(Message) -> ok
%%     Message = string()
%% @doc Logs an error message to disk.
%% @end
%%--------------------------------------------------------------------
error(Msg) when is_list(Msg) ->
    log(error, Msg).

%%--------------------------------------------------------------------
%% @spec warn(Message) -> ok
%%     Message = term()
%% @doc Logs a warning message to disk.
%% @end
%%--------------------------------------------------------------------
warn(Msg) when is_list(Msg) ->
    log(warning, Msg).

%%--------------------------------------------------------------------
%% @spec info(Message) -> ok
%%     Message = term()
%% @doc Logs an informational message disk.
%% @end
%%--------------------------------------------------------------------
info(Msg) when is_list(Msg) ->
    log(info, Msg).

%%--------------------------------------------------------------------
%% @spec console(Message) -> ok
%%     Message = term()
%% @doc Logs a message to the console.
%% @end
%%--------------------------------------------------------------------
console(Msg) when is_list(Msg) ->
    log(console, Msg).

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec init(PropList) -> {ok, State}
%%      PropList = [Property]
%%      Property = {Name, Value}
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%% 
%% == Properties ==
%% <table border="1">
%% <tr>
%%   <th>Name</th>
%%   <th>Type</th>
%%   <th>Description</th>
%%   <th>Default</th>
%% </tr>
%% <tr>
%%  <td>name</td>
%%  <td>string()</td>
%%  <td>Name of the log file, e.g. Name.0, Name.1, Name.N</td>
%%  <td>"log"</td>
%% </tr>
%% <tr>
%%  <td>dir</td>
%%  <td>string()</td>
%%  <td>Directory where the files are stored.</td>
%%  <td>"."</td>
%% </tr>
%% <tr>
%%  <td>max_files</td>
%%  <td>integer()</td>
%%  <td>Maximum number of log files before they are recycled.</td>
%%  <td>5</td>
%% </tr>
%% <tr>
%%  <td>max_bytes</td>
%%  <td>integer()</td>
%%  <td>Maximum number of bytes written before the logger moves to the next file.</td>
%%  <td>10 * 1024 * 1024 (10MB)</td>
%% </tr>
%% </table>
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    console(io_lib:format("Starting ~w...~n", [?MODULE])),
    Name = proplists:get_value(name, Args, "log"),
    Dir = proplists:get_value(dir, Args, "."),
    MaxFiles = proplists:get_value(max_files, Args, 5),
    MaxBytes = proplists:get_value(max_bytes, Args, 10 * 1024 * 1024),
    CurrentNumber = read_index_file(Dir, Name),
    State = open_log_file(#state{name=Name, dir=Dir, max_files=MaxFiles, 
				 max_bytes=MaxBytes,
				 current_number=CurrentNumber}),
    NewState = write_log_file(State, {info, 
				      "Started Logging in GMT time."}),
    {ok, NewState}.

%%--------------------------------------------------------------------
%% @spec
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% @doc Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event. 
%%
%% Preferrably called by gen_event:notify/2 e.g.
%%     gen_event:notify(event_logger, {Class, Msg})
%% Class could be any term, but the following are recommended:
%%     error | warning | info
%% @end
%%--------------------------------------------------------------------
handle_event({Class, Event}, State) ->
    if Class == console ->
	    io:format("==EVENT LOGGED==~n" ++ Event ++ "~n"),
	    NewState = State;
       true ->
	    NewState = write_log_file(State, {Class, Event}),
	    console(Event)
    end,
    {ok, NewState};
handle_event(Event, State) ->
    handle_event({noclass, Event}, State).

%%--------------------------------------------------------------------
%% @spec
%% handle_call(Request, State) -> {ok, Reply, State} |
%%                                {swap_handler, Reply, Args1, State1, 
%%                                  Mod2, Args2} |
%%                                {remove_handler, Reply}
%% @doc Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event 
%% handler to handle the request.
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @spec
%% handle_info(Info, State) -> {ok, State} |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                              remove_handler
%% @doc This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc Whenever an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and 
%% do any necessary cleaning up. 
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
%    NewState = write_log_file(State, {info, "Shutting down ~p (~p)~n", 
%			   [?SERVER, Reason]}),
%    CurrentFD = NewState#state.current_fd,
    CurrentFD = State#state.current_fd,
    file:close(CurrentFD),
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState} 
%% @doc Convert process state when code is changed.
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
read_index_file(Dir, Name) ->
    console(io_lib:format("Reading index file...~n", [])),
    IndexFile = filename:join([Dir, Name ++ ".index"]),
    case file:read_file(IndexFile) of 
	{ok, BinaryIndex} ->
	    binary_to_term(BinaryIndex);
	{error, _reason} ->
	    0
    end.

%% @ spec write_index_file(Dir, Name, Index) -> ok | {error, Reason}
%% @ doc Writes the current log file number to the Name.index file
%% The logger uses this when resuming logging from another session.
%% @end
write_index_file(Dir, Name, Index) when is_number(Index) ->
    console(io_lib:format("Writing to index file...~n", [])),
    IndexFile = filename:join([Dir, Name ++ ".index"]),
    filelib:ensure_dir(IndexFile),
    file:write_file(IndexFile, term_to_binary(Index)).

%% @spec open_log_file(State) -> NewState
%% @doc Takes the current state and opens the file necessary and then
%% returns a new state with the newly opened file descriptor.
%% @end
open_log_file(State) ->
    console(io_lib:format("Opening log file...~n", [])),
    Dir = State#state.dir,
    Name = State#state.name,
    CurrentNumber = State#state.current_number,
    FileName = io_lib:format("~w.~w", [list_to_atom(Name), CurrentNumber]),
    FilePath = filename:join([Dir, FileName]),
    CurrentBytes = case filelib:is_file(FilePath) of
		       true ->
			   filelib:file_size(FilePath);
		       false ->
			   0
		   end,
    filelib:ensure_dir(FilePath),
    {ok, FD} = file:open(FilePath, [append, delayed_write, raw]),
    State#state{current_fd=FD, current_bytes=CurrentBytes}.

%% @spec write_log_file(State, {Class, Event}) -> NewState
%% @doc Writes an event to the current log file and returns the new state.
%% It will check to see if the file needs to be rotated and do so.
%% @end
write_log_file(State, {Class, Event}) ->
    CurrentFD = State#state.current_fd,
    CurrentBytes = State#state.current_bytes,
    MaxBytes = State#state.max_bytes,
    {{Y, M, D}, {H, Mi, S}} = erlang:universaltime(),
    Line = io_lib:format(?LINE_FORMAT, [Y, M, D, H, Mi, S, Class, Event]),
    LineBin = list_to_binary(Line),
    LineBytes = byte_size(LineBin),
    NewBytes = LineBytes + CurrentBytes,
    console(io_lib:format("Writing to log file... (~w+~wb/~w)~n",
			      [LineBytes, CurrentBytes, MaxBytes])),
    case NewBytes =< MaxBytes of
	true ->
	    file:write(CurrentFD, LineBin),
	    State#state{current_bytes=NewBytes};
	false ->
	    write_log_file(next_log_file(State), {Class, Event})
    end.

%% @spec next_log_file(State) -> NewState
%% @doc Rotates log files, meaning it will move to Name.N + 1 where N is the current 
%% file number, truncating the next file. If N == the maximum number of file, 
%% it will change N to 0.
%% @end
next_log_file(State) ->
    Dir = State#state.dir,
    Name = State#state.name,
    CurrentNumber = State#state.current_number,
    console(io_lib:format("Opening next log file...(~w)~n", [CurrentNumber])),
    MaxNumber = State#state.max_files,
    NextNumber = case CurrentNumber < MaxNumber-1 of
		     true ->
			 CurrentNumber + 1;
		     false ->
			 0
		 end,
    CurrentFD = State#state.current_fd,
    ok = file:close(CurrentFD),
    NewState = open_log_file(State#state{current_number=NextNumber}),
    file:truncate(NewState#state.current_fd),
    write_index_file(Dir, Name, NextNumber),
    NewState.
    
