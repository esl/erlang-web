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
%%% File	: e_logger.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc Logger module for Erlang Web framework.
%%% @end
%%%-------------------------------------------------------------------
-module(e_logger).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([log/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {enabled,
		log_dir, 
		max_files,
		current_file = 1,
		fd,
		max_entries,
		current_entry = 1}).

-define(SERVER, ?MODULE).
-define(FILENAME, "e_logger.log.").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec(log/1 :: (term()) -> ok).	     
log(Msg) ->
    gen_server:cast(?SERVER, {log, Msg}).

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
    LogDir = e_conf:get_conf({logger, log_dir}, filename:join(e_conf:server_root(), "log")),
    LogRes = case file:make_dir(LogDir) of
		 ok ->
		     ok;
		 {error, eexist} ->
		     ok;
		 Else ->
		     error_logger:error_msg("~p module, error during log directory creation (~p), "
					    "reason: ~p~n", [?MODULE, LogDir, Else]),
		     Else
	     end,
    
    MaxLogs = e_conf:get_conf({logger, max_files}, 5),
    MaxSize = e_conf:get_conf({logger, max_entries}, 1 bsl 10),
    Enabled = e_conf:get_conf({logger, enabled}, true),
    
    if
	LogRes == ok ->
	    Filename = filename:join([LogDir, ?FILENAME ++ "1"]),
	    case file:open(Filename, [delayed_write]) of
		{ok, Fd} ->
		    {ok, #state{max_files = MaxLogs,
				max_entries = MaxSize,
				log_dir = LogDir,
				enabled = Enabled,
				fd = Fd}};
		Error ->
		    error_logger:error_msg("~p module, error during creating the log file (~p), "
					   "reason: ~p~n", [?MODULE, Filename, Error]),
		    {stop, {could_not_open_log_file, Error}}
	    end;
	true ->
	    {stop, {could_not_create_log_dir, LogRes}}
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_, #state{enabled = false} = State) ->
    {noreply, State};

handle_cast({log, Msg}, State) ->
    Result = if
		   State#state.max_entries =< State#state.current_entry ->
		       wrap_log_file(State);
		   true ->
		       {ok, State#state{current_entry = State#state.current_entry+1}}
	       end,

    case Result of
	{ok, NewState} ->
	    log_msg(Msg, NewState#state.fd),
	    
	    {noreply, NewState};
	Else ->
	    {stop, Else, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

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
terminate(_Reason, State) ->
    file:close(State#state.fd).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec(wrap_log_file/1 :: (tuple()) -> {ok, tuple()} | {error, term()}).	     
wrap_log_file(State) ->
    file:close(State#state.fd),
    NewState = if
		   State#state.max_files == State#state.current_file ->
		       State#state{current_file = 1, 
				   current_entry = 1};
		   true ->
		       State#state{current_file = State#state.current_file+1,
				   current_entry = 1}
	       end,

    Filename = filename:join([State#state.log_dir, ?FILENAME++integer_to_list(NewState#state.current_file)]),
    case file:open(Filename, [delayed_write]) of
	{ok, Fd} ->
	    {ok, NewState#state{fd = Fd}};
	Error ->
	    error_logger:error_msg("~p module, error during creating the log file (~p), "
				   "reason: ~p~n", [?MODULE, Filename, Error]),
	    Error
    end.

-spec(log_msg/2 :: (term(), pid()) -> ok).	     
log_msg(Msg, Fd) ->
    Log = lists:flatten(io_lib:format("[~p ~p] ~p~n~n"), [date(), time(), Msg]),
    file:write(Fd, Log).
