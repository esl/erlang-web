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

%%
%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%
%% @doc Main module for processing the annotations.
%% The annotations are processed during the compilation phase.
%% The main reason for them is to provide the Aspects in Erlang 
%% programming language.<br/>
%% So far, the following annotations have been implemented:
%% <ul>
%% <li><i>-backend_call(Fun/Arity)</i> - indicates, that function should
%% be invoked on the backend node, so the necessary RPC 
%% (which is transparent from the user and developer side of view)
%% will be called.</li>
%% <li><i>-invalidate({Fun/Arity, [Regexp]})</i> - defines that
%% right after the function execution, the invalidation of the 
%% regexps should be processed (always! - without inspecting the
%% result of the function). The called function is 
%% e_cluster:invalidate/1</li>
%% <li><i>-invalidate_groups({Fun/Arity, [Group]})</i> - the same
%% as <i>invalidate</i> but invalidating groups instead of ids.</li>
%% <li><i>-invalidate({Fun/Arity, [Regexp], PredFun})</i> - defines
%% that right after execution of the <i>Fun</i>, the predicate 
%% function (<i>PredFun</i>) will be triggered. If the returning value
%% of the predicate function is <i>true</i> then the regular
%% expression invalidation happens.</i>
%% <li><i>-invalidate_groups({Fun/Arity}, [Group], PredFun)</i> - the
%% same as <i>invalidate</i> above.</li>
%% </ul>
%% In order to use the annotations, the precompiler directive
%% <pre>
%% -compile({parse_transform, e_annotation}).
%% </pre>
%% must be attached to the source file.
%% 
-module(e_annotation).

-export([parse_transform/2]).

-define(INVALIDATOR_VAR_NAME, '__INVALIDATOR').
-define(GROUP_INVALIDATOR_VAR_NAME, '__G_INVALIDATOR').
-define(NODE_VAR_NAME, '__NODE_NAME').
-define(RPC_ERROR_NAME, '__RPC_ERROR').
-define(RPC_RESULT, '__RPC_RESULT').

-spec(parse_transform/2 :: (list(tuple()), list()) -> list(tuple())).	     
parse_transform(Tree, _Options) ->
    {NewTree, Annotations} = remove_annotations(Tree),
    transform_tree(NewTree, Annotations).
    
-spec(remove_annotations/1 :: (list(tuple())) -> {list(tuple()), list(tuple())}).	     
remove_annotations(Tree) ->
    remove_annotations(Tree, [], []).

-spec(remove_annotations/3 :: (list(tuple()), list(tuple()), list(tuple())) -> {list(tuple()), list(tuple())}).	      
remove_annotations([{attribute, _, backend_call, Fun} | Rest], Tree, Annotations) ->
    remove_annotations(Rest, Tree, [{backend_call, Fun} | Annotations]);
remove_annotations([{attribute, _, invalidate, {Fun, Regexps}} | Rest], Tree, Annotations) ->
    remove_annotations(Rest, Tree, [{invalidator, Fun, Regexps} | Annotations]);
remove_annotations([{attribute, _, invalidate, {Fun, Regexps, Pred}} | Rest], Tree, Annotations) ->
    remove_annotations(Rest, Tree, [{cond_invalidator, Fun, Regexps, Pred} | Annotations]);
remove_annotations([{attribute, _, invalidate_groups, {Fun, Groups}} | Rest], Tree, Annotations) ->
    remove_annotations(Rest, Tree, [{group_invalidator, Fun, Groups} | Annotations]);
remove_annotations([{attribute, _, invalidate_groups, {Fun, Groups, Pred}} | Rest], Tree, Annotations) ->
    remove_annotations(Rest, Tree, [{cond_group_invalidator, Fun, Groups, Pred} | Annotations]);
remove_annotations([{attribute, _, module, Name} = A | Rest], Tree, Annotations) ->
    put(module_name, Name),
    remove_annotations(Rest, [A | Tree], Annotations);
remove_annotations([Element | Rest], Tree, Annotations) ->
    remove_annotations(Rest, [Element | Tree], Annotations);
remove_annotations([], Tree, Annotations) ->
    {lists:reverse(Tree), Annotations}.
    
-spec(transform_tree/2 :: (list(tuple()), list(tuple())) -> list(tuple())).	     
transform_tree(Tree, Annotations) ->
    transform_tree(Tree, [], Annotations).

-spec(transform_tree/3 :: (list(tuple()), list(tuple()), list(tuple())) -> list(tuple())).	     
transform_tree([{function, _, _, _, _} = Function | Rest], Tree, Annotations) ->
    transform_tree(Rest, [transform_function(Function, Annotations) | Tree], Annotations);
transform_tree([Other | Rest], Tree, Annotations) ->
    transform_tree(Rest, [Other | Tree], Annotations);
transform_tree([], Tree, _) ->
    lists:reverse(Tree).
    
-spec(transform_function/2 :: (tuple(), list(tuple())) -> tuple()).	     
transform_function({function, _, Name, Arity, _} = F, Annotations0) ->
    case lists:filter(fun(Ann) -> 
			      {Name, Arity} == element(2, Ann)
		      end, Annotations0) of
	[] ->
	    F;
	Annotations ->
	    transform_function1(F, Annotations)
    end.

-spec(transform_function1/2 :: (tuple(), list(tuple())) -> tuple()).
transform_function1({function, Line, Name, Arity, Clauses0}, Annotations) ->
    Clauses = lists:map(fun(Clause) ->
				element(2, lists:foldl(fun transform_clause/2, {Name, Clause}, Annotations))
			end, Clauses0),

    {function, Line, Name, Arity, Clauses}.

-spec(transform_clause/2 :: (tuple(), list(tuple())) -> tuple()).	     
transform_clause({backend_call, _}, {Name, C}) ->
    {Name, transform_backend_call(C, Name)};
transform_clause({invalidator, _, Regexps}, {Name, C}) ->
    {Name, transform_invalidator(C, Regexps)};
transform_clause({group_invalidator, _, Groups}, {Name, C}) ->
    {Name, transform_group_invalidator(C, Groups)};
transform_clause({cond_invalidator, _, Regexps, Pred}, {Name, C}) ->
    {Name, transform_invalidator(C, Regexps, Pred)};
transform_clause({cond_group_invalidator, _, Groups, Pred}, {Name, C}) ->
    {Name, transform_group_invalidator(C, Groups, Pred)}.

-spec(transform_backend_call/2 :: (tuple(), atom()) -> tuple()).	     
transform_backend_call({clause, Line, Args, Guards, Body}, FunName) ->
    FirstLine = element(2, hd(Body)),
    
    NewBody = [{'case', FirstLine,
		{call, FirstLine,
		 {remote, FirstLine, 
		  {atom, FirstLine, application},
		  {atom, FirstLine, get_env}
		 },
		 [{atom, FirstLine, eptic},
		  {atom, FirstLine, node_type}]
		},
		[{clause, FirstLine, 
		  [{tuple, FirstLine, 
		    [{atom, FirstLine, ok},
		     {atom, FirstLine, frontend}]
		   }],
		  [],
 		  [{match, FirstLine, 
 		    {tuple, FirstLine,
 		     [{atom, FirstLine, ok},
 		      {var, FirstLine, ?NODE_VAR_NAME}]
 		    },
 		    {call, FirstLine, 
 		     {remote, FirstLine, 
 		      {atom, FirstLine, application},
 		      {atom, FirstLine, get_env}
 		     },
 		     [{atom, FirstLine, eptic_fe},
 		      {atom, FirstLine, be_server_name}]
 		    }
 		   },
		   {'case', FirstLine, 
 		    {call, FirstLine,
 		     {remote, FirstLine,
 		      {atom, FirstLine, rpc},
 		      {atom, FirstLine, call}
 		     },
 		     [{var, FirstLine, ?NODE_VAR_NAME},
 		      {atom, FirstLine, get(module_name)},
 		      {atom, FirstLine, FunName},
 		      prepare_arguments_list(Args, FirstLine)]
 		    },
 		    [{clause, FirstLine, 
 		      [{tuple, FirstLine, 
 			[{atom, FirstLine, badrpc},
 			 {var, FirstLine, ?RPC_ERROR_NAME}]
 		       }],
 		      [],
 		      [{call, FirstLine, 
 			{remote, FirstLine, 
 			 {atom, FirstLine, error_logger},
 			 {atom, FirstLine, error_msg}
 			},
 			[{string, FirstLine, "~p module, error during RPC call to backend, reason: ~p~n"},
 			 {cons, FirstLine, 
 			  {atom, FirstLine, get(module_name)},
 			  {cons, FirstLine, 
 			   {var, FirstLine, ?RPC_ERROR_NAME},
 			   {nil, FirstLine}
 			  }
 			 }]
 		       },
 		       {tuple, FirstLine, 
 			[{atom, FirstLine, badrpc},
 			 {var, FirstLine, ?RPC_ERROR_NAME}]
 		       }]
 		     },
 		     {clause, FirstLine, 
 		      [{var, FirstLine, ?RPC_RESULT}],
 		      [],
		      [{var, FirstLine, ?RPC_RESULT}]
 		     }]
 		   }
		  ]
		 },
		 {clause, FirstLine, 
		  [{var, FirstLine, '_'}],
		  [],
		  Body
		 }]
		}],
    
    {clause, Line, Args, Guards, NewBody}.

-spec(transform_invalidator/2 :: (tuple(), list(tuple())) -> tuple()).	     
transform_invalidator(Clause, Regexps) ->
    transform_gen_invalidator(Clause, invalidate, ?INVALIDATOR_VAR_NAME, Regexps).

-spec(transform_group_invalidator/2 :: (tuple(), list(tuple())) -> tuple()).	
transform_group_invalidator(Clause, Groups) ->
    transform_gen_invalidator(Clause, invalidate_groups, ?GROUP_INVALIDATOR_VAR_NAME, Groups).

-spec(transform_invalidator/3 :: (tuple(), list(tuple()), atom() | {atom(), atom()}) -> tuple()).	     
transform_invalidator(Clause, Regexps, Pred) ->
    transform_gen_invalidator(Clause, invalidate, ?INVALIDATOR_VAR_NAME, Regexps, Pred).

-spec(transform_group_invalidator/3 :: (tuple(), list(tuple()), atom() | {atom(), atom()}) -> tuple()).	
transform_group_invalidator(Clause, Groups, Pred) ->
    transform_gen_invalidator(Clause, invalidate_groups, ?GROUP_INVALIDATOR_VAR_NAME, Groups, Pred).


-spec(transform_gen_invalidator/4 :: (tuple(), atom(), atom(), list(string())) -> tuple()).	     
transform_gen_invalidator({clause, Line, Args, Guards, Body}, Fun, Var, Regexps) ->
    FirstLine = element(2, hd(Body)),
    LastLine = element(2, hd(lists:reverse(Body))),
    
    NewBody = [{match, FirstLine, 
		{var, FirstLine, Var},
		{block, FirstLine, Body}
	       },
	       {call, LastLine, 
		{remote, LastLine,
		 {atom, LastLine, e_cluster},
		 {atom, LastLine, Fun}},
		[prepare_list_of_strings(Regexps, LastLine)]
	       },
	       {var, LastLine, Var}],

    {clause, Line, Args, Guards, NewBody}.

-spec(transform_gen_invalidator/5 :: (tuple(), atom(), atom(), list(string()), atom() | {atom(), atom()}) -> tuple()).
transform_gen_invalidator({clause, Line, Args, Guards, Body}, Fun, Var, Regexps, Pred) ->
    FirstLine = element(2, hd(Body)),
    LastLine = element(2, hd(lists:reverse(Body))),

    NewBody = [{match, FirstLine, 
		{var, FirstLine, Var},
		{block, FirstLine, Body}
	       },
	       {'case', LastLine,
		{call, LastLine,
		 case Pred of
		     {M, F} when is_atom(M), is_atom(F) ->
			 {remote, LastLine,
			  {atom, LastLine, M},
			  {atom, LastLine, F}
			 };
		     _ when is_atom(Pred) ->
			 {atom, LastLine, Pred}
		 end,
		 [{var, LastLine, Var}]
		},
		[{clause, LastLine,
		  [{atom, LastLine, true}],
		  [],
		  [{call, LastLine, 
		    {remote, LastLine,
		     {atom, LastLine, e_cluster},
		     {atom, LastLine, Fun}
		    },
		    [prepare_list_of_strings(Regexps, LastLine)]
		   }]
		 },
		 {clause, LastLine,
		  [{var, LastLine, '_'}],
		  [],
		  [{atom, LastLine, ok}]
		 }]
	       },
	       {var, LastLine, Var}],
    
    {clause, Line, Args, Guards, NewBody}.
		
-spec(prepare_arguments_list/2 :: (list(tuple()), integer()) -> tuple()).	     
prepare_arguments_list([H | R], Line) ->
    {cons, Line,
     H,
     prepare_arguments_list(R, Line)};
prepare_arguments_list([], Line) ->
    {nil, Line}.

-spec(prepare_list_of_strings/2 :: (list(string()), integer()) -> tuple()).	     
prepare_list_of_strings([H | R], Line) ->
    {cons, Line, 
     {string, Line, H},
     prepare_list_of_strings(R, Line)};
prepare_list_of_strings([], Line) ->
    {nil, Line}.
