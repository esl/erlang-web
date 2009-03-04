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
%% Ltd. Portions created by Erlang Training & Consulting Ltd are Copyright 2008,
%% Erlang Training & Consulting Ltd. All Rights Reserved.

%%%-------------------------------------------------------------------
%%% File    : e_session.erl
%%% @author Lukas Larsson <lukas@erlang-consulting.com>
%%% @doc Handling of sessions for the eptic webplatform.<br/>
%%% A lot of ideas and some code is stolen from yaws_session_server which are 
%%% copyrighted (c) 2006, Claes Wikstrom, klacke@hyber.org. 
%%% All rights reserved.
%%% @end
%%% Created : 25 Jun 2008 by Lukas Larsson <lukas@erlang-consulting.com>
%%%-------------------------------------------------------------------
-module(e_session).

-behaviour(gen_server).

%% API
-export([start_link/0,stop/0,new_session/1,update_session/2,get_session/1,
	 delete_session/1,print_sessions/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("eptic/include/eptic.hrl").

-record(e_session,{ cookie,
		    created,
		    expires,
		    extend_time,
		    data }).

-define(SESSION_TABLE,e_session).
-define(SESSION_SERVER,e_session_server).
-define(TIMEOUT,120000). % 2 min timeout

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%% @hidden
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SESSION_SERVER}, ?MODULE, [], []).

%% @hidden
stop() ->
    gen_server:call(?SESSION_SERVER,stop).

%%
%% @spec new_session(SessionData :: term()) -> string()
%% @doc Initializes the new session with the <i>SessionData</i> and generates the cookie.
%% The randomly generated cookie is returned.
%%
-spec(new_session/1 :: (term()) -> string()).	     
new_session(Data) ->
    new_session(Data,?ExtendTime_default).
-spec(new_session/2 :: (term(), integer()) -> string()).	     
new_session(Data,ExpireTime) ->
    gen_server:call(?SESSION_SERVER,{new_session,Data,ExpireTime}).

%%
%% @spec update_session(Cookie :: term(), NewData :: term()) -> true | {error, no_prev_session}
%% @doc Updates the session bound to the <i>Cookie</i> with the <i>NewData</i>.
%% If no session with the <i>Cookie</i> has been found,
%% <i>{error, no_prev_session}</i> is returned.
%%
-spec(update_session/2 :: (term(), term()) -> true | {error, no_prev_session}).	     
update_session(Cookie,NewData) ->
    case ets:lookup(?SESSION_TABLE,Cookie) of
	[Val] ->
	    Val2 = Val#e_session{
		     expires = gnow() + Val#e_session.extend_time,
		     data = NewData},
	    ets:insert(?SESSION_TABLE,Val2);
	[] ->
	    {error, no_prev_session}
    end.

%%
%% @spec get_session(Cookie :: term()) -> {ok, undefined} | {ok, term()}
%% @doc Retrives the session state from the internal data storage.
%% Updates also the session expire time.<br/>
%% Returns the previous session state or the tuple <i>{ok, undefined}</i>
%% if no previous session has been found.
%%
-spec(get_session/1 :: (term()) -> {ok, undefined} | {ok, term()}).	     
get_session(Cookie) ->
    case ets:lookup(?SESSION_TABLE,Cookie) of
	[#e_session{ data = Data } = E] ->
	    E2 = E#e_session{expires = gnow() + E#e_session.extend_time},
	    ets:insert(?SESSION_TABLE,E2),
	    {ok,Data};
	[] ->
	    {ok,undefined}
    end.

%%
%% @spec delete_session(Cookie :: term()) -> nil()
%% @doc Removes the session from the session table.
%% Returns new, empty cookie.
%%
-spec(delete_session/1 :: (term()) -> nil()).	     
delete_session(Cookie) ->
    ets:delete(?SESSION_TABLE,Cookie),
    "". %% Return a new empty cookie.

%%
%% @spec print_sessions() -> ok
%% @doc Prints all kept sessions on the Erlang system shell.
%% 
-spec(print_sessions/0 :: () -> ok).	     
print_sessions() ->
    lists:foreach(
      fun(#e_session{ cookie = C, 
		      created = Created, 
		      expires = Expires,
		      extend_time = Extend,
		      data = Data }) ->
	      io:format("Cookie = ~p~n"
			"Created = ~p~n"
			"Expires = ~p~n"
			"Extend Time = ~p~n"
			"Data = ~p~n",[C,Created,Expires,Extend,Data])
      end,ets:tab2list(?SESSION_TABLE)).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%% @hidden
%%--------------------------------------------------------------------
init(_Args) ->
    {A,B,C} = now(),
    random:seed(A,B,C),
    ets:new(?SESSION_TABLE,[set,named_table,public,{keypos,2}]),
    {ok, undefined, ?TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @hidden
%%--------------------------------------------------------------------
handle_call({new_session,Data,ExtendTime},_From,_State) ->
    Now = gnow(),
    N = random:uniform(16#ffffffffffffffff), %% 64 bits
    TS = calendar:local_time(),
    C = atom_to_list(node()) ++ [$-|integer_to_list(N)],
    NS = #e_session{cookie = C,
		    created = TS,
		    data = Data,
		    expires = Now + ExtendTime,
		    extend_time = ExtendTime},
    ets:insert(?MODULE, NS),
    {reply, C, undefined, ?TIMEOUT};
handle_call(stop, _From, State) ->
    {stop, stopped, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%% @hidden
%%--------------------------------------------------------------------
handle_info(timeout, State) ->
    trav_ets(),
    {noreply, State,?TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @hidden
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%% @hidden
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

trav_ets() ->
    N = gnow(),
    trav_ets(N, ets:first(?SESSION_TABLE)).

trav_ets(_N, '$end_of_table') ->
    ok;
trav_ets(N, Key) ->
    case ets:lookup(?SESSION_TABLE, Key) of
	[E] ->
	    if
		E#e_session.expires > N ->
		    trav_ets(N, ets:next(?SESSION_TABLE, Key));
		true ->
		    Next = ets:next(?SESSION_TABLE, Key),
		    ets:delete(?SESSION_TABLE, Key),
		    trav_ets(N, Next)
		    
	    end;
	[] ->
	   trav_ets(N, ets:next(?SESSION_TABLE, Key))
    end.

gnow() ->
    calendar:datetime_to_gregorian_seconds(
      calendar:local_time()).
