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
%%% File    : e_db_mnesia.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc An interface to the Mnesia database engine.
%%% This module should not be used explicitly, it should be only called from
%%% the <i>e_db</i>.<br/>
%%% All the calls are transactional.
%%% @see e_db
%%% @end
%%%-------------------------------------------------------------------

-module(e_db_mnesia).
-export([install/0]).
-export([read/1, read/2, delete/2, write/2, update/2, size/1, get_next_id/1]).

-include_lib("stdlib/include/qlc.hrl").

-record(id, {table, next_id}).

%%
%% @spec install() -> none()
%% @doc Creates a table named <i>ids_tab</i> responsible for  storing the unique ids for elements.
%% @todo Change node() to some proper be_nodes() or similar.
%%
-spec(install/0 :: () -> none()).	     
install() ->
    mnesia:create_table(ids_tab, [{attributes, record_info(fields, id)},
				  {disc_copies, [node()]}]).


%%
%% @spec read(Type :: atom()) -> [Element] | {error, Reason}
%%   Element = tuple()
%% @equiv e_db:read/1
%%	    
-spec(read/1 :: (atom() | string()) -> [tuple()] | {error, any()}).
read(Tab) ->
    Trans = fun() ->
		    qlc:e(qlc:q([El || El <- mnesia:table(Tab)]))
	    end,
    
    case mnesia:transaction(Trans) of
	{atomic, Ret} ->
	    Ret;
	{aborted, Reason} ->
	    {error, Reason}
    end.

%%
%% @spec read(Type :: atom(), Id :: any()) -> Element :: tuple() | not_found | {error, Reason}
%% @equiv e_db:read/2
%%
-spec(read/2 :: (atom(), integer() | string()) -> tuple() | not_found | {error, any()}).
read(Tab, Key) ->
    Trans = fun() ->
		    mnesia:read(Tab, Key, read)
	    end,

    case mnesia:transaction(Trans) of
	{atomic, [El]} ->
	    El;
	{atomic, []} ->
	    not_found;
	{aborted, Reason} ->
	    {error, Reason}
    end.

%%
%% @spec delete(Type :: atom(), Element :: tuple()) -> ok | {error, Reason}
%% @equiv e_db:delete/2
%% 
-spec(delete/2 :: (atom(), tuple()) -> ok | {error, any()}).  
delete(Tab, Key) ->
    Trans = fun() ->
		    mnesia:delete(Tab, Key, write)
	    end,
    
    case mnesia:transaction(Trans) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    {error, Reason}
    end.

%%
%% @spec write(Type :: atom(), Element :: tuple()) -> ok | {error, Reason}
%% @equiv e_db:write/2
%%
-spec(write/2 :: (atom(), tuple()) -> ok | {error, any()}).
write(Tab, Element) ->
    Trans = fun() ->
		    mnesia:write(Tab, Element, write)
	    end,
    
    case mnesia:transaction(Trans) of
	{atomic, ok} ->
	    ok;
	{aborted, Reason} ->
	    {error, Reason}
    end.

%%
%% @spec update(Type :: atom(), Element :: tuple()) -> ok | {error, Reason}
%% @equiv e_db:update/2
%%
-spec(update/2 :: (atom(), tuple()) -> ok | {error, any()}).
update(Tab, Element) ->		    
    write(Tab, Element).

%%
%% @spec size(Type :: atom()) -> integer() | {error, Reason}
%% @equiv e_db:size/1
%%
-spec(size/1 :: (atom()) -> integer() | {error, any()}).
size(Tab) ->
    mnesia:table_info(Tab, size).

%%
%% @spec get_next_id(Type :: atom()) -> integer() | {error, Reason}
%% @equiv e_db:get_next_id/1
%%
-spec(get_next_id/1 :: (atom()) -> integer() | {error, any()}).	
get_next_id(Tab) ->
    mnesia:dirty_update_counter(ids_tab, Tab, 1).
