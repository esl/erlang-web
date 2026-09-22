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
%%% File    : e_dict_test.erl
%%% Author  : Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% Description : 
%%%
%%% Created : 26 Oct 2009 by michalptaszek <michalptaszek@coltrane.erlangsystems.com>
%%%-------------------------------------------------------------------
-module(e_dict_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ewts/include/request.hrl").

single_level_store_test() ->
    e_dict:init_state([]),
    ?assert(e_dict:fset("test", "test")),
    ?assert(e_dict:fset("abc", 123)),
    ?assert(e_dict:fset("aaa", {this, is, a, test})),
    
    {ok, Dict} = e_dict:get_state(),
    ?assertEqual(lists:sort(Dict),
		 lists:sort([{"test", "test"},
			     {"abc", 123},
			     {"aaa", {this, is, a, test}}])).

double_level_store_test() ->
    e_dict:init_state([]),
    ?assert(e_dict:fset("test", "test")),
    ?assert(e_dict:fset("abc", 123)),
    ?assert(e_dict:fset("aaa", {this, is, a, test})),
    ?assert(e_dict:fset("1:2", second_level)),
    ?assert(e_dict:fset("1:3", third_level)),
    ?assert(e_dict:fset("test2:wow", 0)),
    
    {ok, Dict} = e_dict:get_state(),
    ?assertEqual(lists:sort(Dict),
		 lists:sort([{"test", "test"},
			     {"abc", 123},
			     {"aaa", {this, is, a, test}},
			     {"1", [{"2", second_level},
				    {"3", third_level}]},
			     {"test2", [{"wow", 0}]}])).

multi_level_store_test() ->
    e_dict:init_state([]),
    ?assert(e_dict:fset("test", "test")),
    ?assert(e_dict:fset("abc", 123)),
    ?assert(e_dict:fset("aaa", {this, is, a, test})),
    ?assert(e_dict:fset("1:2", second_level)),
    ?assert(e_dict:fset("1:3", third_level)),
    ?assert(e_dict:fset("test2:wow", 0)),
    ?assert(e_dict:fset("a:b:c:d:e", f)),
    
    {ok, Dict} = e_dict:get_state(),
    ?assertEqual(lists:sort(Dict),
		 lists:sort([{"test", "test"},
			     {"abc", 123},
			     {"a", [{"b", 
				     [{"c",
				       [{"d",
					 [{"e", f}]}]}]}]},
			     {"aaa", {this, is, a, test}},
			     {"1", [{"2", second_level},
				    {"3", third_level}]},
			     {"test2", [{"wow", 0}]}])).

get_test() ->
    e_dict:init_state([]),
    ?assert(e_dict:fset("test", "test")),
    ?assert(e_dict:fset("abc", 123)),
    ?assert(e_dict:fset("aaa", {this, is, a, test})),
    ?assert(e_dict:fset("1:2", second_level)),
    ?assert(e_dict:fset("1:3", third_level)),
    ?assert(e_dict:fset("test2:wow", 0)),
    ?assert(e_dict:fset("a:b:c:d:e", f)),
    
    {ok, Dict} = e_dict:get_state(),
    ?assertEqual(lists:sort(Dict),
		 lists:sort([{"test", "test"},
			     {"abc", 123},
			     {"a", [{"b", 
				     [{"c",
				       [{"d",
					 [{"e", f}]}]}]}]},
			     {"aaa", {this, is, a, test}},
			     {"1", [{"2", second_level},
				    {"3", third_level}]},
			     {"test2", [{"wow", 0}]}])),
    
    ?assertEqual("test", e_dict:fget("test")),
    ?assertEqual(123, e_dict:fget("abc")),
    ?assertEqual({this, is, a, test}, e_dict:fget("aaa")),
    ?assertEqual([{"2", second_level}, {"3", third_level}], e_dict:fget("1")),
    ?assertEqual(second_level, e_dict:fget("1:2")),
    ?assertEqual(third_level, e_dict:fget("1:3")),
    ?assertEqual(f, e_dict:fget("a:b:c:d:e")).

delete_test() ->
    e_dict:init_state([]),
    ?assert(e_dict:fset("test", "test")),
    ?assert(e_dict:fset("abc", 123)),
    ?assert(e_dict:fset("aaa", {this, is, a, test})),
    ?assert(e_dict:fset("1:2", second_level)),
    ?assert(e_dict:fset("1:3", third_level)),
    ?assert(e_dict:fset("test2:wow", 0)),
    ?assert(e_dict:fset("a:b:c:d:e", f)),
    ?assert(e_dict:fdelete("abc")),
    ?assert(e_dict:fdelete("1:2")),
    ?assert(e_dict:fdelete("test2")),
    
    {ok, Dict} = e_dict:get_state(),
    ?assertEqual(lists:sort(Dict),
		 lists:sort([{"test", "test"},
			     {"a", [{"b", 
				     [{"c",
				       [{"d",
					 [{"e", f}]}]}]}]},
			     {"aaa", {this, is, a, test}},
			     {"1", [{"3", third_level}]}])).

