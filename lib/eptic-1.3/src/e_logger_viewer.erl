%% The contents of this file are subject to the Erlang Web Public License,
%% Version 1.0, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Web Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang-consulting.com/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Erlang Training & Consulting
%% Ltd. Portions created by Erlang Training & Consulting Ltd are Copyright 2009,
%% Erlang Training & Consulting Ltd. All Rights Reserved.

%%%-------------------------------------------------------------------
%%% File    : e_logger_viewer.erl
%%% Author  : Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% Description : Viewer for the logs recorded by the e_logger facility.
%%%
%%% Created : 14 Apr 2009 by Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%%-------------------------------------------------------------------
-module(e_logger_viewer).

-behaviour(gen_server).

%% API
-export([start/0, stop/0]).
-export([read_log_dir/0, read_log_dir/1, read_log_file/1]).
-export([clear/0, set_mode/1]).
-export([list/0, list/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {entries = [], 
		running_mode = shell}).

-define(FILENAME, "e_logger.log.").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
-spec(start/0 :: () -> {ok, pid()} | ignore | {error, term()}).	     
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

-spec(stop/0 :: () -> ok).	     
stop() ->
    gen_server:call(?MODULE, stop).

-spec(read_log_dir/0 :: () -> ok | {error, term()}).	     
read_log_dir() ->
    read_log_dir(e_conf:get_conf({logger, log_dir}, 
				 filename:join(e_conf:server_root(), "log"))).

-spec(read_log_dir/1 :: (string()) -> ok | {error, term()}). 
read_log_dir(Dir) ->
    gen_server:call(?MODULE, {read_log_dir, Dir}).

-spec(read_log_file/1 :: (string()) -> ok | {error, term()}). 
read_log_file(File) ->
    gen_server:call(?MODULE, {read_log_file, File}).

-spec(clear/0 :: () -> ok).	    
clear() ->
    gen_server:cast(?MODULE, clear).

-spec(set_mode/1 :: (shell | list) -> ok).	     
set_mode(Mode) ->
    gen_server:cast(?MODULE, {set_mode, Mode}).

-spec(list/0 :: () -> list() | ok).	     
list() ->
    gen_server:call(?MODULE, list).

-spec(list/1 :: (integer()) -> list() | ok).	     
list(RequestId) ->
    gen_server:call(?MODULE, {list, RequestId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    process_flag(priority, low),

    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({read_log_dir, Dir}, _, State) ->
    LogFiles = lists:map(fun(F) -> 
				 filename:join([Dir, F])
			 end, filelib:wildcard(?FILENAME ++ "*", Dir)),
    {Results, Entries} = load_log_file(LogFiles, State#state.entries, []),

    case lists:any(fun(Res) ->
			   Res =/= ok
		   end, Results) of
	true ->
	    {reply, {error, Results}, State#state{entries = Entries}};
	false ->
	    {reply, ok, State#state{entries = Entries}}
    end;

handle_call({read_log_file, File}, _, State) ->
    {[Result], Entries} = load_log_file([File], State#state.entries, []),

    {reply, Result, State#state{entries = Entries}};

handle_call(list, _, State) ->
    if
	State#state.running_mode == shell ->
	    print_list(State#state.entries),
	    {reply, ok, State};
	true ->
	    {reply, State#state.entries, State}
    end;

handle_call({list, Rid}, _, State) ->
    if
	State#state.running_mode == shell ->
	    print_list(filter(Rid, State#state.entries, [])),
	    {reply, ok, State};
	true ->
	    {reply, filter(Rid, State#state.entries, []), State}
    end;

handle_call(stop, _, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(clear, State) ->
    {noreply, State#state{entries = []}};

handle_cast({set_mode, Mode}, State) 
  when Mode == shell; Mode == list ->
    {noreply, State#state{running_mode = Mode}}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec(load_log_file/3 :: (list(string()), list(), list()) -> {list(), list()}).
load_log_file([Filename | Files], Entries, Results) ->
    case file:consult(Filename) of
	{ok, Logs} ->
	    load_log_file(Files, [Entries | Logs], [ok | Results]);
	Else ->
	    error_logger:warning_msg("~p module, error during reading the "
				     "log file ~p, reason: ~p.~n Skipping that "
				     "log file.~n~n",
				     [?MODULE, Filename, Else]),
	    load_log_file(Files, Entries, [{Filename, Else} | Results])
    end;
load_log_file([], Entries, Results) ->
    {lists:reverse(Results), lists:flatten(Entries)}.

-spec(filter/3 :: (integer(), list(), list()) -> list()).	     
filter(Rid, [{Rid, _, _} = Log | Logs], Results) ->
    filter(Rid, Logs, [Log | Results]);
filter(Rid, [_ | Logs], Results) ->
    filter(Rid, Logs, Results);
filter(_, [], Results) ->
    lists:reverse(Results).

-spec(print_list/1 :: (list()) -> any()).	     
print_list(Logs) ->
    print_header(),
    print_log_list(Logs),
    print_footer().

-spec(print_log_list/1 :: (list()) -> ok).	     
print_log_list([{Rid, Time, Event} | Logs]) ->
    io:format("~s~s   ~s~n", [string:right(integer_to_list(Rid), 5, $ ),
			      string:right(format_date(calendar:now_to_local_time(Time)), 22, $ ),
			      lists:flatten(io_lib:format("~p", [Event]))]),
    print_log_list(Logs);
print_log_list([]) ->
    ok.

-spec(print_header/0 :: () -> ok).	     
print_header() ->
    io:format("~s~n", [string:centre("", 80, $=)]),
    io:format("~s~s   ~s~n", [string:right("RID", 5, $ ),
			      string:right("Date", 22, $ ),
			      string:right("Event", 50, $ )]),
    io:format("~s~n~n", [string:centre("", 80, $=)]).

-spec(print_footer/0 :: () -> ok).	     
print_footer() ->
    io:format("~n~s~n~n", [string:centre("", 80, $=)]).

format_date({{Y, Mo, D}, {H, Mi, S}}) ->
    lists:flatten(io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:"
				"~2.2.0w:~2.2.0w", 
				[Y,Mo,D,H,Mi,S])).
