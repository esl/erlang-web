-module(cache_performance_test).

-export([cache_test/0]).

-define(ADDRESSES,
	["404.html",
	 "about.html",
	 "crud.html",
	 "base.html",
	 "getting_started_quickly.html",
	 "main_page.html",
	 "klacke.html",
	 "enoent.html",
	 "downloads.html",
	 "filled_base.html",
	 "501.html",
	 "users/create_users.html",
	 "users/update_users.html",
	 "login/warning.html"]).

-define(RETRIES, 10000).

cache_test() ->
    e_dict:init_state([]),
    
    {A, B, C} = now(),
    random:seed(A, B, C),
    
    T1 = now(),
    test_file_cache(?RETRIES, ?ADDRESSES, length(?ADDRESSES)),
    T2 = now(),
    DT1 = timer:now_diff(T2, T1),
    
    T3 = now(),
    test_ets_cache(?RETRIES, ?ADDRESSES, length(?ADDRESSES)),
    T4 = now(),
    DT2 = timer:now_diff(T4, T3),

    io:format("File cache: ~p~nEts cache: ~p~n", [DT1, DT2]).
    
test_file_cache(0, _, _) ->
    ok;
test_file_cache(N, Addresses, Len) ->
    e_cache:read_file(lists:nth(random:uniform(Len), Addresses)),
    test_file_cache(N-1, Addresses, Len).

test_ets_cache(0, _, _) ->
    ok;
test_ets_cache(N, Addresses, Len) ->
    e_ets_cache:read_file(lists:nth(random:uniform(Len), Addresses)),
    test_ets_cache(N-1, Addresses, Len).

