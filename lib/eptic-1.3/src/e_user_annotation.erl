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
%%% File    : e_user_annotation.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc Module responsible for embedding the user-defined annotations in the code.
%%% @end
%%% 
%%% Created : 22 Apr 2009 by Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%%-------------------------------------------------------------------
-module(e_user_annotation).

-export([parse_transform/2]).

parse_transform(Tree, _Options) ->
    transform_tree(Tree, [], [], []).

transform_tree([{attribute, _, module, Name} = A | Rest], Tree, [], []) ->
    put(module_name, Name),
    transform_tree(Rest, [A | Tree], [], []);
transform_tree([{attribute, _, ew_user_annotation, {Args, before, Mod, Func}} | Rest], Tree, Before, After) ->
    transform_tree(Rest, Tree, [{Args, Mod, Func} | Before], After);
transform_tree([{attribute, _, ew_user_annotation, {Args, 'after', Mod, Func}} | Rest], Tree, Before, After) ->
    transform_tree(Rest, Tree, Before, [{Args, Mod, Func} | After]);
transform_tree([F | Rest], Tree, [], []) ->
    transform_tree(Rest, [F | Tree], [], []);
transform_tree([{function, _, _, _, _} = F | Rest], Tree, Before, After) ->
    NewF = transform_function(F, lists:reverse(Before), lists:reverse(After)),
    transform_tree(Rest, [NewF | Tree], [], []);
transform_tree([Element | Rest], Tree, Before, After) ->
    transform_tree(Rest, [Element | Tree], Before, After);
transform_tree([], Tree, _, _) ->
    lists:reverse(Tree).

transform_function({function, Line, FunName, Arity, Clauses}, Before, After) ->
    put(function_name, FunName),
    put(function_arity, Arity),
    NewClauses = transform_clause(Clauses, Before, After, []),
    {function, Line, FunName, Arity, NewClauses}.

transform_clause([OrgClause | Rest], Before, After, Clauses) ->
    BeforeClause = transform_clause_before(OrgClause, Before),
    AfterClause = transform_clause_after(BeforeClause, After),
    transform_clause(Rest, Before, After, [AfterClause | Clauses]);
transform_clause([], _, _, Clauses) ->
    lists:reverse(Clauses).

transform_clause_before(Clause, []) ->
    Clause;
transform_clause_before({clause, L, CArgs, Guards, Body}, Annotations0) ->
    TgtFunM = get(module_name),
    TgtFunN = get(function_name),
    Arity = get(function_arity),

    Annotations = prepare_annotations(Annotations0, L),

    AFunName = get_unique_atom(),
    FuncArgs = get_unique_atom(),
    AArgs = get_unique_atom(),
    Mod = get_unique_atom(),
    Func = get_unique_atom(),
    Rest = get_unique_atom(),
    Self = get_unique_atom(),
    NewArgs = get_unique_atom(),
    EMod = get_unique_atom(),
    EFunc = get_unique_atom(),

    NewBody = [{match, L,
		{var, L, AFunName},
		{'fun', L,
		 {clauses,
		  [{clause, L,
		    [{var, L, FuncArgs},
		     {cons, L,
		      {tuple, L,
		       [{var, L, AArgs}, {var, L, Mod}, {var, L, Func}]},
		      {var, L, Rest}},
		     {var, L, Self}],
		    [],
		    [{'case', L,
		      {call, L,
		       {remote, L, {var, L, Mod}, {var, L, Func}},
		       [{var, L, AArgs},
			{atom, L, TgtFunM},
			{atom, L, TgtFunN},
			{var, L, FuncArgs}]},
		      [{clause, L,
			[{tuple, L, [{atom, L, proceed}, {var, L, NewArgs}]}],
			[],
			[{call, L,
			  {var, L, Self},
			  [{var, L, NewArgs}, {var, L, Rest}, {var, L, Self}]}]},
		       {clause, L, 
			[{tuple, L, [{atom, L, skip}, {var, L, NewArgs}]}],
			[],
			[{var, L, NewArgs}]},
		       {clause, L,
			[{tuple, L,
			  [{atom, L, error},
			   {tuple, L,
			    [{var, L, EMod},
			     {var, L, EFunc},
			     {var, L, NewArgs}]}]}],
			[],
			[{call, L, 
			  {atom, L, apply},
			  [{var, L, EMod},
			   {var, L, EFunc},
			   {var, L, NewArgs}]}]}]}]},
		   {clause, L, 
		    [{var, L, FuncArgs}, {nil, L}, {var, L, '_'}],
		    [],
		    [{'if', L,
		      [{clause, L, [],
			[[{op, L, '=/=',
			   {call, L, {atom, L, length}, [{var, L, FuncArgs}]},
			   {integer, L, Arity}}]],
			[{call, L,
			  {atom, L, throw},
			  [{tuple, L,
			    [{atom, L, bad_annotation_result_length},
			     {call, L,
			      {atom, L, length},
			      [{var, L, FuncArgs}]}]}]}]},
		       {clause, L, [],
			[[{atom, L, true}]],
			[{call, L,
			  {atom, L, apply},
			  [{'fun', L,
			    {clauses, 
			     [{clause, L,
			       CArgs,
			       [],
			       Body}]}},
			   {var, L, FuncArgs}]}]}]}]}]}}},
	       {call, L, 
		{var, L, AFunName},
		[prepare_arguments_list(CArgs, L),
		 Annotations,
		 {var, L, AFunName}]}],
    
    {clause, L, CArgs, Guards, NewBody}.

transform_clause_after(Clause, []) ->
    Clause;
transform_clause_after({clause, L, CArgs, Guards, Body}, Annotations0) ->
    TgtFunM = get(module_name),
    TgtFunN = get(function_name),

    Annotations = prepare_annotations(Annotations0, L),

    OrgFunName = get_unique_atom(),
    AFunName = get_unique_atom(),
    FuncResult = get_unique_atom(),
    AArgs = get_unique_atom(),
    Mod = get_unique_atom(),
    Func = get_unique_atom(),
    Rest = get_unique_atom(),
    Self = get_unique_atom(),
    NewResult = get_unique_atom(),
    EMod = get_unique_atom(),
    EFunc = get_unique_atom(),

    NewBody = [{match, L,
		{var, L, OrgFunName},
		{'fun', L, 
		 {clauses,
		  [{clause, L, [], [],
		    Body}]}}},
	       {match, L, 
		{var, L, AFunName},
		{'fun', L,
		 {clauses,
		  [{clause, L,
		    [{var, L, FuncResult},
		     {cons, L,
		      {tuple, L,
		       [{var, L, AArgs}, {var, L, Mod}, {var, L, Func}]},
		      {var, L, Rest}},
		     {var, L, Self}],
		    [],
		    [{'case', L,
		      {call, L,
		       {remote, L, {var, L, Mod}, {var, L, Func}},
		       [{var, L, AArgs},
			{atom, L, TgtFunM},
			{atom, L, TgtFunN},
			{var, L, FuncResult}]},
		      [{clause, L,
			[{tuple, L, [{atom, L, proceed}, {var, L, NewResult}]}],
			[],
			[{call, L,
			  {var, L, Self},
			  [{var, L, NewResult}, {var, L, Rest}, {var, L, Self}]}]},
		       {clause, L, 
			[{tuple, L, [{atom, L, skip}, {var, L, NewResult}]}],
			[],
			[{var, L, NewResult}]},
		       {clause, L,
			[{tuple, L,
			  [{atom, L, error},
			   {tuple, L,
			    [{var, L, EMod},
			     {var, L, EFunc},
			     {var, L, NewResult}]}]}],
			[],
			[{call, L, 
			  {atom, L, apply},
			  [{var, L, EMod},
			   {var, L, EFunc},
			   {var, L, NewResult}]}]}]}]},
		   {clause, L, 
		    [{var, L, FuncResult}, {nil, L}, {var, L, '_'}],
		    [],
		    [{var, L, FuncResult}]}]}}},
	       {call, L,
		{var, L, AFunName},
		[{call, L,
		  {var, L, OrgFunName}, []},
		 Annotations,
		 {var, L, AFunName}]}],
    
    {clause, L, CArgs, Guards, NewBody}.

-spec(prepare_arguments_list/2 :: (list(tuple()), integer()) -> tuple()).	     
prepare_arguments_list([H | R], Line) ->
    {cons, Line, H,
     prepare_arguments_list(R, Line)};
prepare_arguments_list([], Line) ->
    {nil, Line}.
    
-spec(get_unique_atom/0 :: () -> atom()).	     
get_unique_atom() ->
    list_to_atom(lists:flatten(io_lib:format("~w", [now()]))).

prepare_annotations(Annotations, Line) ->
    NewLines = [$\n || _ <- lists:seq(1, Line-1)],
    SAnnotations = NewLines ++ lists:flatten(io_lib:format("~w.", [Annotations])),
    {ok, Tokens, _} = erl_scan:string(SAnnotations),
    {ok, [Parsed]} = erl_parse:parse_exprs(Tokens),
    
    Parsed.
