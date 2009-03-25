%% the contents of this file are subject to the erlang web public license,
%% version 1.0, (the "license"); you may not use this file except in
%% compliance with the license. you should have received a copy of the
%% erlang web public license along with this software. if not, it can be
%% retrieved via the world wide web at http://www.erlang-consulting.com/.
%%
%% software distributed under the license is distributed on an "as is"
%% basis, without warranty of any kind, either express or implied. see
%% the license for the specific language governing rights and limitations
%% under the license.
%%
%% the initial developer of the original code is erlang training & consulting
%% ltd. portions created by erlang training & consulting ltd are copyright 2008,
%% erlang training & consulting ltd. all rights reserved.

%%%-------------------------------------------------------------------
%%% file    : e_fe_session.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%%-------------------------------------------------------------------
-module(e_fe_session).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, save_session/1, load_session/1,
	 delete_session/1, get_session/0]).

-export([fget/1, fset/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(e_session,{ cookie,
		    created,
		    expires,
		    extend_time,
		    data }).

-define(SESSION_TABLE,?MODULE).
-define(SESSION_SERVER,e_fe_session_server).
-define(SESSION_NAME,?MODULE).
-define(SESSION_COOKIE,session_cookie).
-define(TIMEOUT,120000). % 2 min timeout

%%====================================================================
%% API
%%====================================================================
%% @hidden
start_link() ->
    gen_server:start_link({local, ?SESSION_SERVER}, ?MODULE, [], []).

%% @hidden
stop() ->
    gen_server:call(?SESSION_SERVER,stop).

%%
%% @spec save_session(NewData :: term()) -> true | {error, no_prev_session}
%% @doc Saves the session with the <i>NewData</i>.
%% If no session corresponding to the current connection has been found,
%% <i>{error, no_prev_session}</i> is returned.
%%
-spec(save_session/1 :: (term()) -> true | {error, no_prev_session}).
save_session(NewData) ->
    Cookie = get(?SESSION_COOKIE),
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
%% @spec load_session(Cookie :: term()) -> Result :: ok | undefined
%% @doc Loads the session state from the internal data storage.
%% Updates also the session expire time.<br/>
%% The loaded session could be accessed by calling {@link fget/1}
%% or {@link fset/1} functions.<br/>
%% Returns ok if the previous session state has been found or <i>undefined</i>
%% otherwise.
%%
-spec(load_session/1 :: (term()) -> undefined | term()).
load_session(Cookie) ->
    put(?SESSION_COOKIE, Cookie),
    case ets:lookup(?SESSION_TABLE,Cookie) of
	[#e_session{ data = Data } = E] ->
	    E2 = E#e_session{expires = gnow() + E#e_session.extend_time},
	    ets:insert(?SESSION_TABLE, E2),
	    put(?SESSION_NAME, Data),

	    ok;
	[] ->
	    undefined
    end.

%%
%% @spec get_session() -> Session :: term()
%% @doc Returns the session stored on the frontend node.
%%
-spec(get_session/0 :: () -> term()).	     
get_session() ->
    get(?SESSION_NAME).

%%
%% @spec delete_session(Cookie :: term()) -> NewCookie :: nil()
%% @doc Removes the session from the session table.
%% Returns new, empty cookie.
%%
-spec(delete_session/1 :: (term()) -> nil()).	     
delete_session(Cookie) ->
    ets:delete(?SESSION_TABLE,Cookie),
    "". %% Return a new empty cookie.

%%
%% @spec fget(Key :: term()) -> Val :: term() | undefined
%% @doc Fetches the value stored under the <i>Key</i> from the attached session.
%% The <i>undefined</i> value is returned if either
%% session is not attached or the <i>Key</i> has no
%% corresponding value.
%% 
-spec(fget/1 :: (term()) -> undefined | term()).	     
fget(Key) ->
    case get(?SESSION_NAME) of
	undefined ->
	    undefined;
	Dict ->
	    case dict:find(Key, Dict) of
		{ok, Val} -> 
		    Val;
		error ->
		    undefined
	    end
    end.

%%
%% @spec fset(Key :: term(), Value :: term()) -> ok | undefined
%% @doc Saves the <i>Value</i> under the <i>Key</i> in the user session.
%% The modified session will be propagated to the backend server.<br/>
%% The function returns ok when modifing session has succeeded, 
%% undefined otherwise.
%%
-spec(fset/2 :: (term(), term()) -> undefined | ok).	     
fset(Key, Val) ->
    case get(?SESSION_NAME) of
	undefined ->
	    undefined;
	Dict ->
	    put(?SESSION_NAME, dict:store(Key, Val, Dict)),
	    ok
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================
%% @hidden
init(_Args) ->
    {A,B,C} = now(),
    random:seed(A,B,C),
    ets:new(?SESSION_TABLE,[set,named_table,public,{keypos,2}]),
    {ok, undefined, ?TIMEOUT}.

%% @hidden
handle_call(stop, _From, State) ->
    {stop, stopped, State}.

%% @hidden
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @hidden
handle_info(timeout, State) ->
    trav_ets(),
    {noreply, State, ?TIMEOUT}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
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
