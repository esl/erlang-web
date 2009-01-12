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
%%% file    : e_fe_gc.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%%-------------------------------------------------------------------
-module(e_fe_gc).
-export([start_link/0, init/0]).
-export([collect_timeout/1, clear_timeout_cache/0]).

%% @hidden
start_link() ->
    Pid = spawn_link(?MODULE, init, []),
    register(?MODULE, Pid),
    {ok, Pid}.

%% @hidden
init() ->
    timer:apply_after(60000, e_fe_gc, clear_timeout_cache, []),
    case ets:lookup(frontend_conf, gc_mode) of
	[] ->
	    timer:apply_after(60000, e_fe_gc, collect_timeout, [60000]),
	    timeout_loop();
	[{_, {timeout, Mins}}] ->
	    timer:apply_after(60000 * Mins, e_fe_gc, collect_timeout, [60000 * Mins]),
	    timeout_loop();
	[{_, {hits, Hits}}] ->
	    hits_loop(Hits, Hits)
    end.

-spec(timeout_loop/0 :: () -> none()).	     
timeout_loop() ->
    receive
	_ ->
	    timeout_loop()
    end.

-spec(hits_loop/2 :: (integer(), integer()) -> none()).	     
hits_loop(0, Default) ->
    collect(),
    hits_loop(Default, Default);
hits_loop(Hits, Default) ->
    receive
	hit ->
	    hits_loop(Hits-1, Default)
    end.

-spec(collect/0 :: () -> swap).	     
collect() ->
    {_, Second} = e_fe_cache:get_order(),
    ets:delete_all_objects(Second),
    
    e_fe_cache ! swap.

-spec(collect_timeout/1 :: (integer()) -> none()).	     
collect_timeout(T) ->
    collect(),
    timer:apply_after(T, e_fe_gc, collect_timeout, [T]).

-spec(clear_timeout_cache/2 :: (integer(), list(tuple())) -> ok).	     
clear_timeout_cache(_, []) ->
    ok;
clear_timeout_cache(Time, [{URL, _, MaxTime, InsertionTime} | Rest]) ->
    Diff = timer:now_diff(Time, InsertionTime),
    if
	Diff div 1000000 >= MaxTime ->
	    ets:delete(cache_timeout, URL);
	true ->
	    ok
    end,
    
    clear_timeout_cache(Time, Rest).

-spec(clear_timeout_cache/0 :: () -> none()).	     
clear_timeout_cache() ->
    Now = now(),
    Cached = ets:tab2list(cache_timeout),
    
    clear_timeout_cache(Now, Cached),
    
    timer:apply_after(60000, e_fe_gc, clear_timeout_cache, []).
