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
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc Module responsible for managing the request dictionary.
%%% @end
%%%-------------------------------------------------------------------
-module(e_dict).

%% API
-export([init_state/1, terminate_state/0, get_state/0]).
-export([fset/2, fset/3, fget/1, fget/2, fget0/2, fget/3, finsert/2, finsert/3]).
-export([fdelete/1, fdelete/2]).

-export([start_link/0]).
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%%====================================================================
%% Exported functions
%%====================================================================
%% @hidden
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% @hidden
init_state(List) when is_list(List) ->
    ets:insert(?MODULE, {self(), List});
init_state(Dict) when is_tuple(Dict) ->
    ets:insert(?MODULE, {self(), dict:to_list(Dict)}).

%% @hidden
-spec(get_state/0 :: () -> {ok, list()} | undefined).	     
get_state() ->
    case ets:lookup(?MODULE, self()) of
	[{_, Dict}] ->
	    {ok, Dict};
	_ ->
	    undefined
    end.

%% @hidden
terminate_state() ->
    ets:delete(?MODULE, self()).

%% 
%% @spec fset(Key :: string(), Value :: term()) -> true
%% @doc Inserts the <i>Value</i> under the specified <i>Key</i> in the request dictionary.
%%
-spec(fset/2 :: (string(), term()) -> true).
fset(Key, Value) ->
    fset0(string:tokens(Key, [$:]), Value).

%%
%% @spec fset(List :: string(), Key :: string(), Value :: term()) -> true
%% @doc Appends the <i>Value</i> into the <i>List</i> under the <i>Key</i> in the request dictionary.
%% <i>List</i> is the top-level key to the proplist in the dictionary. 
%% If there is no list, the new one is created - the call is equivalent to:
%% ```eptic:fset(List, {Key, Value})'''
%% If there is a list and it contains the element <i>{Key, Value}</i>
%% then nothing happens.
%% Otherwise, the <i>{Key, Value}</i> is appended to the list and the list is then
%% stored in the request dictionary.
%%
-spec(fset/3 :: (string(), string(), term()) -> true).
fset(List, Key, Value) ->
    fset0([List, Key], Value).

-spec(fset0/2 :: (list(string()), term()) -> true).
fset0(Keys, Value) ->
    case ets:lookup(?MODULE, self()) of
	[{_, Dict}] ->
	    ets:insert(?MODULE, {self(), fset0(Keys, Value, Dict)});
	[] ->
	    exit(no_dict_attached)
    end.

-spec(fset0/3 :: (list(string()), term(), tuple()) -> tuple()).
fset0([Key], Value, Dict) ->
    lists:keystore(Key, 1, Dict, {Key, Value});
fset0([Key | Rest], Value, Dict) ->
    SubDict = case proplists:get_value(Key, Dict) of
		  undefined ->
		      [];
		  S ->
		      S
	      end,
    lists:keystore(Key, 1, Dict, {Key, fset0(Rest, Value, SubDict)}).

%%
%% @spec finsert(List :: term(), Key :: term(), Value :: term()) -> true
%% @doc Inserts the <i>Value</i> into the <i>List</i> under the <i>Key</i> in the request dictionary.
%% <i>List</i> is the top-level key to the container in the dictionary. 
%% There must be a proper container inside the request dictionary.<br/>
%% If there was a tuple with the first element equal to <i>Key</i>, then 
%% the tuple is replaced with <i>{Key, Value}</i>.
%% Otherwise, the <i>{Key, Value}</i> is appended to the <i>List</i>
%%
-spec(finsert/3 :: (term(), term(), term()) -> true).
finsert(List, Key, Value) ->
    fset0([List, Key], Value).

-spec(finsert/2 :: (string(), term()) -> true).
finsert(Key, Value) ->
    fset(Key, Value).
    
%%
%% @spec fget(Key :: string()) -> Value :: term() | undefined
%% @doc Fetches the element kept in the request dictionary under the <i>Key</i>.
%% If there is no element under <i>Key</i> inside the request dictionary,
%% <i>undefined</i> is returned.
%%
-spec(fget/1 :: (string()) -> term()).
fget(Key0) ->
    Key = string:tokens(Key0, [$:]),
    case ets:lookup(?MODULE, self()) of
	[{_, Dict}] -> 
	    fget0(Key, Dict);
	[] -> 
	    exit(no_dict_attached)
    end.

%%
%% @spec fget(List :: string(), Key :: string()) -> term() | undefined
%% @doc Fetches the element kept in the <i>List</i> in the dictionary under the <i>Key</i>.
%% <i>List</i> is top-level key to the container stored in the request dictionary.
%% The values kept in the container under the <i>Key</i> are returned.
%% If there is no values under <i>Key</i> or there is no <i>List</i>
%% in the request dictionary, the <i>undefined</i> is returned.
%%
-spec(fget/2 :: (string(), string()) -> term()).	      
fget(List, Key) ->
    case ets:lookup(?MODULE, self()) of
	[{_, Dict}] -> 
	    fget0([List, Key], Dict);
	[] -> 
	    exit(no_dict_attached)
    end.

-spec(fget0/2 :: (list(string()), tuple()) -> term()).
fget0([Key], Dict) ->
    proplists:get_value(Key, Dict);
fget0([Key | Rest], Dict) ->
    case proplists:get_value(Key, Dict) of
	undefined ->
	    undefined;
	SubDict ->
	    fget0(Rest, SubDict)
    end.

%% @hidden
fget(List, Key, Validator) ->
    case catch Validator(fget(List, Key)) of
	{'EXIT', _Reason} ->
	    exit({validation, Key});
	Result ->
	    Result
    end.

%%
%% @since 1.3
%% @spec fdelete(Key :: term()) -> true
%% @doc Removes the value stored in the request dictionary under the <i>Key</i>
%% The value that is stored under the <i>Key</i> will be lost
%% pernamently. 
%%
-spec(fdelete/1 :: (term()) -> true).	     
fdelete(Key0) ->
    Key = string:tokens(Key0, [$:]),
    case ets:lookup(?MODULE, self()) of
	[{_, Dict}] ->
	    fdelete0(Key, Dict);
	[] ->
	    exit(no_dict_attached)
    end.

%%
%% @since 1.3
%% @spec fdelete(List :: term(), Key :: term()) -> true
%% @doc Removes the value stored under the <i>Key</i> inside the <i>List</i> in the request dictionary
%% If the <i>List</i> after the removal process is empty, it 
%% is also removed.
%%
-spec(fdelete/2 :: (term(), term()) -> true).	     
fdelete(List, Key) ->
    case ets:lookup(?MODULE, self()) of
	[{_, Dict}] ->
	    fdelete0([List, Key], Dict);
	[] ->
	    exit(no_dict_attached)
    end.

-spec(fdelete0/2 :: (list(string()), tuple()) -> true).
fdelete0(Keys, Dict) ->
    ets:insert(?MODULE, {self(), fdelete1(Keys, Dict)}).

-spec(fdelete1/2 :: (list(string()), tuple()) -> tuple()).
fdelete1([Key], Dict) ->
    lists:keydelete(Key, 1, Dict);
fdelete1([Key | Rest], Dict) ->
    case proplists:get_value(Key, Dict) of
	undefined ->
	    Dict;
	SubDict ->
	    lists:keystore(Key, 1, Dict, {Key, fdelete1(Rest, SubDict)})
    end.

%%====================================================================
%% Server functions
%%====================================================================
%% @hidden
init([]) ->
    ets:new(?MODULE, [named_table, public]),
    {ok, not_used}.

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
