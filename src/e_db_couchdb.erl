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

%%%-----------------------------------------------------------------------------
%%% File    : e_db_couchdb.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang.consulting.com>
%%% @doc An interface to the CouchDB database engine.
%%% This module should not be used explicitly, it should be only called from
%%% the <i>e_db</i>.
%%% @see e_db
%%% @end
%%%-----------------------------------------------------------------------------
-module(e_db_couchdb).
-export([install/0, start/0]).
-export([read/1, read/2, delete/2, write/2, update/2, size/1, get_next_id/1]).

%%
%% @spec install() -> ok
%% @doc Sets up the CouchDB database for Erlang Web needs.
%% It creates the separate database (its name will be obtained from 
%% the project name set in <i>project.conf</i>).<br/>
%% The database address is taken from the <i>e_conf:couchdb_address</i>.<br/>
%% It also creates the separate database for the ids (with the suffix "_ids").
%% @end
%% @see e_conf:project_name/0
%% @see e_conf:couchdb_address/0
%%
-spec(install/0 :: () -> ok).	     
install() ->
    application:start(inets),

    Name = e_conf:project_name(),
    Address = e_conf:couchdb_address(),

    case http:request(put, {Address ++ Name ++ "_ids", [], [], []}, [], []) of
	{ok, {{_, 201, _}, _, _}} ->
	    ok;
	{ok, {{_, 409, _}, _, IJson0}} ->
	    {ok, [IJson]} = e_json:decode(IJson0),

	    {_, {_, IError}} = lists:keysearch(error, 1, IJson),
	    {_, {_, IReason}} = lists:keysearch(reason, 1, IJson),
	    
	    if
		IError == "database_already_exists" ->
		    ok;
		true ->
		    error_logger:error_msg("~p module, CouchDB returned an error during install ids tab~n"
					   "Error: ~s~n"
					   "Reason: ~s~n", [?MODULE, IError, IReason])
	    end;
	{error, IReason} ->
	    error_logger:error_msg("~p module, error during install(), reason: ~p~n", [?MODULE, IReason])
    end,

    case http:request(put, {Address ++ Name, [], [], []}, [], []) of
	{ok, {{_, 201, _}, _, _}} ->
	    ok;
	{ok, {{_, 409, _}, _, Json0}} ->
	    {ok, [Json]} = e_json:decode(Json0),

	    {_, {_, Error}} = lists:keysearch(error, 1, Json),
	    {_, {_, Reason}} = lists:keysearch(reason, 1, Json),

	    if
		Error == "database_already_exists" ->
		    ok;
		true ->
		    error_logger:error_msg("~p module, CouchDB returned an error during install ids tab~n"
					   "Error: ~s~n"
					   "Reason: ~s~n", [?MODULE, Error, Reason])
	    end;
	{error, Reason} ->
	    error_logger:error_msg("~p module, error during install(), reason: ~p~n", [?MODULE, Reason])
    end.

%%
%% @spec start() -> ok | {error, Reason :: term()}
%% @doc Starts the inets application so it is possible to make the request to the CouchDB instance.
%%
-spec(start/0 :: () -> ok | {error, term()}).	     
start() ->
    application:start(inets).

%%
%% @spec read(Type :: atom()) -> [Element] | {error, Reason}
%%   Element = tuple()
%% @equiv e_db:read/1
%%	    
-spec(read/1 :: (atom() | string()) -> [tuple()] | {error, any()}).
read(Prefix0) ->
    Name = e_conf:project_name(),
    CouchURL = e_conf:couchdb_address(),

    URL = CouchURL ++ Name,

    Prefix = get_prefix(Prefix0),

    case http:request(URL ++ "/_all_docs") of
	{ok, {{_, 200, _}, _, Json0}} ->
	    {ok, [Json]} = e_json:decode(Json0),
	    
	    {_, {_, N}} = lists:keysearch(total_rows, 1, Json),
	    if
		N > 0 ->
		    {_, {_, Rows}} = lists:keysearch(rows, 1, Json),

		    Ids = lists:filter(fun([{id, E}, _, _]) ->
					       is_prefix(Prefix, E)
				       end, tuple_to_list(Rows)),

		    lists:foldl(fun([{id, Id}, _, _], Acc) ->
					read_row(URL, Id, Acc)
				end, [], Ids);
		true ->
		    []
	    end;
	{error, Reason} ->
	    error_logger:error_msg("~p module, error during read/1, prefix: ~p, reason: ~p~n", [?MODULE, Prefix, Reason]),
	    {error, Reason}
    end.

%%
%% @spec read(Type :: atom(), Id :: any()) -> Element :: tuple() | not_found | {error, Reason}
%% @equiv e_db:read/2
%%
-spec(read/2 :: (atom(), integer() | string()) -> tuple() | not_found | {error, any()}).
read(Prefix0, Id) ->
    Name = e_conf:project_name(),
    CouchURL = e_conf:couchdb_address(),
    Prefix = get_prefix(Prefix0),

    Url = CouchURL ++ Name ++ "/" ++ Prefix ++ get_id(Id),

    case http:request(Url) of
	{ok, {{_, 200, _}, _, Json}} ->
	    {ok, [Element]} = e_json:decode(Json),
	    
	    case lists:keysearch(element, 1, Element) of
		false ->
		    not_found;
		{_, {_, E}} ->
		    E
	    end;
	{ok, {{_, 404, _}, _, _}} ->
	    not_found;
	{error, Reason} ->
	    error_logger:error_msg("~p module, error during read_row/3, id: ~p, reason: ~p~n", [?MODULE, Id, Reason]),
	    {error, Reason}
    end.

%%
%% @spec delete(Type :: atom(), Key :: any()) -> ok | {error, Reason}
%% @equiv e_db:delete/2
%% 
-spec(delete/2 :: (atom(), any()) -> ok | {error, any()}).  
delete(Prefix0, Id) ->
    Name = e_conf:project_name(),
    CouchURL = e_conf:couchdb_address(),
    Prefix = get_prefix(Prefix0),

    Url = CouchURL ++ Name ++ "/" ++ Prefix ++ get_id(Id),
    Rev = get_rev(Url),
    
    case Rev of
	{ok, R} ->
	    case http:request(delete, {Url ++ "?rev=" ++ R, []}, [], []) of
		{ok, {{_, 200, _}, _, _}} ->
		    ok;
		{ok, {{_, 404, _}, _, _}} ->
		    {error, not_found};
		{error, Reason1} ->
		    error_logger:error_msg("~p module, error during delete/2, reason: ~p~n", [?MODULE, Reason1])
	    end;
	Else ->
	    {error, Else}
    end.

%%
%% @spec write(Type :: atom(), Element :: tuple()) -> ok | {error, Reason}
%% @equiv e_db:write/2
%%
-spec(write/2 :: (atom(), tuple()) -> ok | {error, any()}).
write(Prefix0, Element) ->
    Name = e_conf:project_name(),
    CouchURL = e_conf:couchdb_address(),
    Prefix = get_prefix(Prefix0),

    Url = CouchURL ++ Name ++ "/" ++ Prefix ++ get_id(element(2, Element)),

    Json = e_json:encode([{element, Element}, {pub_date, {date(), time()}}]),
    case http:request(put, {Url, [], "application/json", Json}, [], []) of
	{ok, {{_, 201, _}, _, _}} ->
	    ok;
	{ok, {{_, 412, _}, _, Json0}} ->
	    {ok, [RJson]} = e_json:decode(Json0),

	    {_, {_, Error}} = lists:keysearch(error, 1, RJson),
	    {_, {_, Reason}} = lists:keysearch(reason, 1, RJson),
	    
	    {error, {Error, Reason}};
	{ok, {{_, 500, _}, _, Json1}} ->
	    {ok, [RJson]} = e_json:decode(Json1),
	    
	    {_, {_, Error}} = lists:keysearch(error, 1, RJson),
	    {_, {_, Reason}} = lists:keysearch(reason, 1, RJson),
	    
	    {error, {Error, Reason}};
	{error, Reason} ->
	    error_logger:error_msg("~p module, error during write/2, reason: ~p~n", [?MODULE, Reason])
    end.

%%
%% @spec update(Type :: atom(), Element :: tuple()) -> ok | {error, Reason}
%% @equiv e_db:update/2
%%
-spec(update/2 :: (atom(), tuple()) -> ok | {error, any()}).
update(Prefix0, Element) ->
    Name = e_conf:project_name(),
    CouchURL = e_conf:couchdb_address(),
    Prefix = get_prefix(Prefix0),

    Url = CouchURL ++ Name ++ "/" ++ Prefix ++ get_id(element(2, Element)),
    
    case get_rev(Url) of
	{ok, Rev} ->
	    Json = e_json:encode([{element, Element}, {pub_date, {date(), time()}}, {'_rev', Rev}]),
	    case http:request(put, {Url, [], "application/json", Json}, [], []) of
		{ok, {{_, 201, _}, _, _}} ->
		    ok;
		{ok, {_, _, Json0}} ->
		    {ok, [RJson]} = e_json:decode(Json0),
		    
		    {_, {_, Error}} = lists:keysearch(error, 1, RJson),
		    {_, {_, Reason}} = lists:keysearch(reason, 1, RJson),
		    
		    {error, {Error, Reason}};
		{error, Reason} ->
		    error_logger:error_msg("~p module, error during update/2, reason: ~p~n", [?MODULE, Reason])
	    end;
	Else ->
	    {error, Else}
    end.

%%
%% @spec size(Type :: atom()) -> integer() | {error, Reason}
%% @equiv e_db:size/1
%%
-spec(size/1 :: (atom()) -> integer() | {error, any()}).
size(Prefix0) ->
    Name = e_conf:project_name(),
    CouchURL = e_conf:couchdb_address(),
    Prefix = get_prefix(Prefix0),

    URL = CouchURL ++ Name,

    case http:request(URL ++ "/_all_docs") of
	{ok, {{_, 200, _}, _, Json0}} ->
	    {ok, [Json]} = e_json:decode(Json0),
	    
	    {_, {_, N}} = lists:keysearch(total_rows, 1, Json),
	    if
		N > 0 ->
		    {_, {_, Rows}} = lists:keysearch(rows, 1, Json),
		    
		    Ids = lists:filter(fun([{id, E}, _, _]) ->
					       is_prefix(Prefix, E)
				       end, tuple_to_list(Rows)),
		    
		    length(Ids);
		true ->
		    0
	    end;
	{error, Reason} ->
	    error_logger:error_msg("~p module, error during size/1, prefix: ~p, reason: ~p~n", [?MODULE, Prefix, Reason]),
	    {error, Reason}
    end.

%%
%% @spec get_next_id(Type :: atom()) -> integer() | {error, Reason}
%% @equiv e_db:get_next_id/1
%%
-spec(get_next_id/1 :: (atom()) -> integer() | {error, any()}).	
get_next_id(Prefix0) ->
    Name = e_conf:project_name() ++ "_ids",
    CouchURL = e_conf:couchdb_address(),
    Prefix = get_prefix(Prefix0),

    Url = CouchURL ++ Name ++ "/" ++ Prefix,
    
    Ret = case http:request(Url) of
	      {ok, {{_, 200, _}, _, Json}} ->
		  {ok, [Counter]} = e_json:decode(Json),
		  
		  {_, {_, C}} = lists:keysearch(counter, 1, Counter),
		  {_, {_, R}} = lists:keysearch('_rev', 1, Counter),
		  
		  {C, R};
	      {ok, {{_, 404, _}, _, _}} ->
		  new;
	      {error, Reason} ->
		  error_logger:error_msg("~p module, error during get_next_id/1, prefix: ~p, reason: ~p~n", [?MODULE, Prefix, Reason]),
		  {error, Reason}
	  end,
    
    case Ret of
	new ->
	    NewCounter = e_json:encode([{counter, 1}]),
	    case http:request(put, {Url, [], "application/json", NewCounter}, [], []) of
		{ok, {{_, 201, _}, _, _}} ->
		    1;
		{ok, {_, _, EJson}} ->
		    {ok, [RJson]} = e_json:decode(EJson),
		    
		    {_, {_, Error0}} = lists:keysearch(error, 1, RJson),
		    {_, {_, Reason0}} = lists:keysearch(reason, 1, RJson),

		    {error, {Error0, Reason0}};
		{error, Reason1} ->
		    error_logger:error_msg("~p module, error during get_next_id/1, prefix: ~p, reason: ~p~n", [?MODULE, Prefix, Reason1]),
		    {error, Reason1}
	    end;
	{N, Rev} ->
	    NewCounter = e_json:encode([{counter, N+1}, {'_rev', Rev}]),
	    case http:request(put, {Url, [], "application/json", NewCounter}, [], []) of
		{ok, {{_, 201, _}, _, _}} ->
		    N+1;
		{ok, {_, _, EJson}} ->
		    {ok, [RJson]} = e_json:decode(EJson),
		    
		    {_, {_, Error0}} = lists:keysearch(error, 1, RJson),
		    {_, {_, Reason0}} = lists:keysearch(reason, 1, RJson),
		    
		    {error, {Error0, Reason0}};
		{error, Reason1} ->
		    error_logger:error_msg("~p module, error during get_next_id/1, prefix: ~p, reason: ~p~n", [?MODULE, Prefix, Reason1]),
		    {error, Reason1}
	    end
    end.
	    
get_rev(Url) ->
    case http:request(Url) of
	{ok, {{_, 200, _}, _, Json}} ->
	    {ok, [Element]} = e_json:decode(Json),
	    
	    case lists:keysearch('_rev', 1, Element) of
		false ->
		    not_found;
		{_, {_, Re}} ->
		    {ok, Re}
	    end;
	{ok, {{_, 404, _}, _, _}} ->
	    not_found;
	{error, Reason} ->
	    error_logger:error_msg("~p module, error during get_rev/1, Url: ~p, reason: ~p~n", [?MODULE, Url, Reason]),
	    Reason
    end.

get_prefix(Prefix) when is_list(Prefix) ->
    Prefix ++ "_";
get_prefix(Prefix) when is_atom(Prefix)  ->
    atom_to_list(Prefix) ++ "_".

get_id(Id) when is_integer(Id) ->
    integer_to_list(Id);
get_id(Id) when is_list(Id) ->
    Id.

is_prefix([], _) ->
    true;
is_prefix([P | PRest], [P | ERest]) ->
    is_prefix(PRest, ERest);
is_prefix(_, _) ->
    false.

read_row(URL, Id, Acc) ->
    case http:request(URL ++ "/" ++ Id) of
	{ok, {{_, 200, _}, _, Json}} ->
	    {ok, [Element]} = e_json:decode(Json),

	    case lists:keysearch(element, 1, Element) of
		false ->
		    Acc;
		{_, {_, E}} ->
		    [E | Acc]
	    end;
	{ok, {{_, 404, _}, _, _}} ->
	    Acc;
	{error, Reason} ->
	    error_logger:error_msg("~p module, error during read_row/3, id: ~p, reason: ~p~n", [?MODULE, Id, Reason]),
	    Acc
    end.
