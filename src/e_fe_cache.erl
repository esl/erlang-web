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
%%% File    : e_fe_cache.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%%-------------------------------------------------------------------
-module(e_fe_cache).

-export([start_link/0, init/0, clear/0]).
-export([request/1, get_order/0]).
-export([dispatcher_reload/1, check_cache/1]).
-export([invalidate_handler/1]).
-export([ask_front_end/1, ask_back_end/3]).
-export([save_cache/2, save_cache/4]).

%% @hidden
start_link() ->
    Pid = spawn_link(?MODULE, init, []),
    register(?MODULE, Pid),

    {ok, Pid}.

%% @hidden
init() ->
    ets:new(cache_persistent, [named_table, public]),
    ets:new(cache_a, [named_table, public]),
    ets:new(cache_b, [named_table, public]),
    ets:new(cache_timeout, [named_table, public]),
    
    loop({cache_a, cache_b}).

-spec(clear/0 :: () -> none()).	     
clear() ->
    [ets:delete_all_objects(Cache) || Cache <- [cache_persistent, cache_a, cache_b, cache_timeout]].

-spec(loop/1 :: ({atom(), atom()}) -> none()).	     
loop({A, B}) ->
    receive
	{Pid, get_order} ->
	    Pid ! {order, {A, B}},
	    loop({A, B});
	swap ->
	    loop({B, A});
	{invalidate, Regexp} ->
	    invalidate(Regexp),
	    loop({A, B});
	{invalidate_groups, Groups} ->
	    invalidate_groups(Groups),
	    loop({A, B})
    end.

-spec(request/1 :: (binary()) -> {cached, term()} | not_found).	     
request(URL) ->
    e_fe_gc ! hit,
    case e_dict:fget("__cacheable") of
	true ->
	    check_cache(URL);
	false ->
	    not_found
    end.

-spec(invalidate_handler/1 :: (list(tuple())) -> ok).	     
invalidate_handler(ToInv) ->
    lists:foreach(fun invalidate/1, ToInv).

-spec(invalidate/1 :: (tuple()) -> ok).	     
invalidate(Regexp) ->
    lists:foreach(fun(Cache) ->
			  invalidate(Cache, Regexp)
		  end, [cache_persistent, cache_a, cache_b, cache_timeout]).

-spec(invalidate/2 :: (atom(), tuple()) -> ok).	     
invalidate(Cache, Regexp) ->
    First = ets:first(Cache),
    invalidate(Cache, First, Regexp).

-spec(invalidate/3 :: (atom(), term(), tuple()) -> ok).	     
invalidate(_, '$end_of_table', _) ->
    ok;
invalidate(Cache, BKey, Regexp) ->
    Key = binary_to_list(BKey),
    Next = ets:next(Cache, BKey),
    case re:run([$/|Key], Regexp,[{capture,first}]) of
	nomatch ->
	    invalidate(Cache, Next, Regexp);
	{match, _} ->
	    ets:delete(Cache, BKey),
	    invalidate(Cache, Next, Regexp);
	{error, Reason} ->
	    error_logger:error_msg("~p module, unknown regexp has come: ~p, error: ~p", 
				   [?MODULE, Regexp, Reason])
    end.

-spec(invalidate_groups/1 :: (list(string())) -> ok).	     
invalidate_groups(Groups) ->
    lists:foreach(fun invalidate_group/1, Groups).

-spec(invalidate_group/1 :: (string()) -> ok).	     
invalidate_group(Group) ->
    lists:foreach(fun(Cache) ->
			  invalidate_group(Cache, Group)
		  end, [cache_persistent, cache_a, cache_b, cache_timeout]).

-spec(invalidate_group/2 :: (atom(), string()) -> ok).
invalidate_group(Cache, Group) ->
    First = ets:first(Cache),
    invalidate_group(Cache, First, Group).

-spec(invalidate_group/3 :: (atom(), term(), string()) -> ok).
invalidate_group(_, '$end_of_table', _) ->	     
    ok;
invalidate_group(Cache, Key, Group) ->
    Next = ets:next(Cache, Key),
    case ets:lookup(Cache, Key) of
	[{_, Groups, _}] -> 
	    case lists:member(Group, Groups) of
		true ->
		    ets:delete(Cache, Key);
		false ->
		    ok
	    end;
	[{_, Groups, _, _, _}] ->
	    case lists:member(Group, Groups) of
		true ->
		    ets:delete(Cache, Key);
		false ->
		    ok
	    end;
	[] ->
	    ok
    end,
    invalidate_group(Cache, Next, Group).

-spec(ask_front_end/1 :: (string()) -> term()).	     
ask_front_end(View) ->
    e_fe_mod_gen:view(View).

-spec(ask_back_end/3 :: (atom(), atom(), list()) -> term()).	   
ask_back_end(M, F, A) ->
    e_fe_proxy:request(M, F, A).

-spec(save_cache/2 :: (string(), binary()) -> ok).	     
save_cache(Id, Cache) ->
    case e_dict:fget("__cacheable") of
	true ->
	    save_cache(eptic:fget("__cache_type"), eptic:fget("__cache_groups"), 
		       list_to_binary(Id), term_to_binary(Cache));
	false ->
	    ok
    end.

-spec(save_cache/4 :: (atom(), list(string()), binary(), binary()) -> ok).
save_cache(no_cache, _, _, _) ->
    ok;
save_cache(normal, Groups, Id, Cache) ->
    save_normal_cache(Id, Groups, Cache);
save_cache(persistent, Groups, Id, Cache) ->
    save_persistent_cache(Id, Groups, Cache);
save_cache({timeout, T}, Groups, Id, Cache) ->
    save_timeout_cache(Id, Groups, Cache, T).

-spec(check_cache/1 :: (binary()) -> {cached, term()} | not_found).	     
check_cache(Key) ->
    case check_cache(cache_persistent, Key) of
	{cached, Cached} ->
	    {cached, Cached};
	not_found ->
	    check_timeout_cache(Key)
    end.

-spec(check_timeout_cache/1 :: (binary()) -> {cached, term()} | not_found).	     
check_timeout_cache(Key) ->
    case ets:lookup(cache_timeout, Key) of
	[{_, _, Bin, _, _}] ->
	    {cached, binary_to_term(Bin)};
	[] ->
	    check_transient_cache(Key)
    end.

-spec(check_cache/2 :: (atom(), binary()) -> {cached, term()} | not_found).    
check_cache(Type, Key) ->
    case ets:lookup(Type, Key) of
	[{_, _, Bin}] ->
	    {cached, binary_to_term(Bin)};
	[] ->
	    not_found
    end.

-spec(check_transient_cache/1 :: (binary()) -> {cached, term()} | not_found).	     
check_transient_cache(Key) ->
    {First, Second} = get_order(),

    case check_cache(Second, Key) of
	{cached, Cached} ->
	    {cached, Cached};
	not_found ->
	    check_cache(First, Key)
    end.					

-spec(save_normal_cache/3 :: (binary(), list(string()), atom()) -> none()).	     
save_normal_cache(Key, Groups, Cache) ->
    {First, _} = get_order(),
    ets:insert(First, {Key, Groups, Cache}).

-spec(save_persistent_cache/3 :: (binary(), list(string()), term()) -> none()).	     
save_persistent_cache(Key, Groups, Cache) ->
    ets:insert(cache_persistent, {Key, Groups, Cache}).

-spec(save_timeout_cache/4 :: (binary(), list(string()), term(), integer()) -> none()).	     
save_timeout_cache(Key, Groups, Cache, Time) ->
    ets:insert(cache_timeout, {Key, Groups, Cache, Time, now()}).

-spec(get_order/0 :: () -> {atom(), atom()}).	     
get_order() ->
    e_fe_cache ! {self(), get_order},
    receive
	{order, Order} ->
	    Order
    end.

-spec(dispatcher_reload/1 :: (list(tuple())) -> none()).	     
dispatcher_reload([{_, S}, {_, D}]) ->
    All = S ++ D,
    Rules = dispatcher_parse(All),

    ets:delete_all_objects(cache_dispatcher),
    ets:insert(cache_dispatcher, Rules).

-spec(dispatcher_parse/1 :: (list(tuple())) -> list(tuple())).	     
dispatcher_parse(List) ->
    dispatcher_parse(List, []).

-spec(dispatcher_parse/2 :: (list(tuple()), list(tuple())) -> list(tuple())).	     
dispatcher_parse([], Acc) ->
    lists:reverse(Acc);
dispatcher_parse([{_, Regexp, _} | Rest], Acc) ->
    dispatcher_parse(Rest, [{Regexp, normal} | Acc]);
dispatcher_parse([{_, Regexp, _, Opts} | Rest], Acc) ->
    case lists:keysearch(cache, 1, Opts) of
	false ->
	    dispatcher_parse(Rest, [{Regexp, normal} | Acc]);
	{_, {_, Type}} ->
	    dispatcher_parse(Rest, [{Regexp, Type} | Acc])
    end.
