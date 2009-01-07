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
%%% File    : e_dict.erl
%%% @author Martin Carlson <martin@erlang-consulting.com>
%%% @doc Module responsible for managing the request dictionary.
%%% @end
%%%-------------------------------------------------------------------
-module(e_dict).

%% API
-export([
	init_state/1,
	terminate_state/0,
	fset/2,
	fset/3,
	fget/1,
	fget/2,
	fget/3,
        finsert/3
]).

-export([start_link/0]).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

-record(state, {}).

%%====================================================================
%% Exported functions
%%====================================================================
%% @hidden
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% @hidden
init_state(Dict) ->
    ets:insert(?MODULE, {self(), dict:from_list(Dict)}).

%% @hidden
terminate_state() ->
    ets:delete(?MODULE, self()).

%% 
%% @spec fset(Key :: term(), Value :: term()) -> none()
%% @doc Inserts the <i>Value</i> under the specified <i>Key</i> in the request dictionary.
%%
-spec(fset/2 :: (term(), term()) -> none()).	     
fset(Key, Value) ->
    case ets:lookup(?MODULE, self()) of
	[{_, Dict}] -> ets:insert(?MODULE, {self(), dict:store(Key, Value, Dict)});
	[]          -> exit(no_dict_attached)
    end.

%%
%% @spec fset(List :: term(), Key :: term(), Value :: term()) -> none()
%% @doc Appends the <i>Value</i> into the <i>List</i> under the <i>Key</i> in the request dictionary.
%% <i>List</i> is the top-level key to the proplist in the dictionary. 
%% If there is no list, the new one is created - the call is equivalent to:
%% ```eptic:fset(List, {Key, Value})'''
%% If there is a list and it contains the element <i>{Key, Value}</i>
%% then nothing happens.
%% Otherwise, the <i>{Key, Value}</i> is appended to the list and the list is then
%% stored in the request dictionary.
%%
-spec(fset/3 :: (term(), term(), term()) -> none()).
fset(List, Key, Value) ->
    case ets:lookup(?MODULE, self()) of
	[{_, Dict}] ->
	    Current = dict_fetch(List, Dict),
	    check_store({Key, Value},Current,List,Dict);
	[]          -> 
	    exit(no_dict_attached)
    end.

check_store(T, undefined, List, Dict) ->
    ets:insert(?MODULE, {self(), dict:store(List,[T], Dict)});

check_store(T, Current, List, Dict) ->
    New = case lists:member(T,Current) of
	      true -> Current;
	      false -> [T|Current]
	  end,
    ets:insert(?MODULE, {self(), dict:store(List, New, Dict)}).

%%
%% @spec finsert(List :: term(), Key :: term(), Value :: term()) -> none()
%% @doc Inserts the <i>Value</i> into the <i>List</i> under the <i>Key</i> in the request dictionary.
%% <i>List</i> is the top-level key to the container in the dictionary. 
%% There must be a proper container inside the request dictionary.<br/>
%% If there was a tuple with the first element equal to <i>Key</i>, then 
%% the tuple is replaced with <i>{Key, Value}</i>.
%% Otherwise, the <i>{Key, Value}</i> is appended to the <i>List</i>
%%
-spec(finsert/3 :: (term(), term(), term()) -> none()).
finsert(List, Key, Value) ->
	case ets:lookup(?MODULE, self()) of
		[{_, Dict}] ->
                    Current = dict_fetch(List, Dict),
                    New = case lists:keysearch(Key,1,Current) of
                            {value, _} -> lists:keyreplace(Key,1,Current,{Key,Value});
                            false -> [{Key,Value}|Current]
                        end,
                    ets:insert(?MODULE, {self(), dict:store(List, New, Dict)});
		[]          -> exit(no_dict_attached)
	end.

%%
%% @spec fget(Key :: term()) -> Value :: term() | undefined
%% @doc Fetches the element kept in the request dictionary under the <i>Key</i>.
%% If there is no element under <i>Key</i> inside the request dictionary,
%% <i>undefined</i> is returned.
%%
-spec(fget/1 :: (term()) -> term()).	      
fget(Key) ->
	case ets:lookup(?MODULE, self()) of
		[{_, Dict}] -> dict_fetch(Key, Dict);
		[]          -> exit(no_dict_attached)
	end.

%%
%% @spec fget(List :: term(), Key :: term()) -> term() | undefined
%% @doc Fetches the element kept in the <i>List</i> in the dictionary under the <i>Key</i>.
%% <i>List</i> is top-level key to the container stored in the request dictionary.
%% The values kept in the container under the <i>Key</i> are returned.
%% If there is no values under <i>Key</i> or there is no <i>List</i>
%% in the request dictionary, the <i>undefined</i> is returned.
%%
-spec(fget/2 :: (term(), term()) -> term()).	      
fget(List, Key) ->
	case fget(List) of
		PropList when is_list(PropList) ->
                    case proplists:get_all_values(Key, PropList) of
                        [] -> undefined;
                        [Value] -> Value;
                        Values -> Values  
                    end;
		Result ->
                    Result
	end.


%% @hidden
fget(List, Key, Validator) ->
    case catch Validator(fget(List, Key)) of
		{'EXIT', _Reason} ->
			exit({validation, Key});
		Result ->
			Result
	end.

%%====================================================================
%% Server functions
%%====================================================================
%% @hidden
init([]) ->
    ets:new(?MODULE, [named_table, public]),
    {ok, #state{}}.

%% @hidden
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

%% @hidden
handle_cast(_Req, State) ->
    {noreply, State}.

%% @hidden
handle_info(_Msg, State) ->
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    ets:delete(?MODULE),
    ok.

%% @hidden
code_change(_VSN, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================
dict_fetch(Key, Dict) ->
	case dict:find(Key, Dict) of
		{ok, Value} -> Value;
		error       -> undefined
	end.	
