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
%% <li><i>-backend_call()</i> - indicates, that function should
%% be invoked on the backend node, so the necessary RPC 
%% (which is transparent from the user and developer side of view)
%% will be called.</li>
%% <li><i>-invalidate([Regexp])</i> - defines that
%% right after the function execution, the invalidation of the 
%% regexps should be processed (always! - without inspecting the
%% result of the function). The called function is 
%% e_cluster:invalidate/1</li>
%% <li><i>-invalidate_groups([Group])</i> - the same
%% as <i>invalidate</i> but invalidating groups instead of ids.</li>
%% <li><i>-invalidate({[Regexp], PredFun})</i> - defines
%% that right after execution of the <i>Fun</i>, the predicate 
%% function (<i>PredFun</i>) will be triggered. If the returning value
%% of the predicate function is <i>true</i> then the regular
%% expression invalidation happens.</i>
%% <li><i>-invalidate_groups({[Group], PredFun})</i> - the
%% same as <i>invalidate</i> above.</li>
%% </ul>
%% In order to use the annotations, the precompiler directive
%% <pre>
%% -compile({parse_transform, e_annotation}).
%% </pre>
%% must be attached to the source file.
%% By default it is included in every entry of the user's application
%% inside the Emakefile and accessible with bin/compile.erl.
%% 
-module(e_annotation).

-export([parse_transform/2]).

-spec(parse_transform/2 :: (list(tuple()), list()) -> list(tuple())).	     
parse_transform(Tree, _Options) ->
    transform_tree(Tree).
    
-spec(transform_tree/1 :: (list(tuple())) -> list(tuple())).	     
transform_tree(Tree) ->
    transform_tree(Tree, [], []).

-spec(transform_tree/3 :: (list(tuple()), list(tuple()), list(tuple() | atom())) -> list(tuple())).
transform_tree([{attribute, _, backend_call, _} | Rest], Tree, Annotations) ->
    transform_tree(Rest, Tree, [backend_call | Annotations]);
transform_tree([{attribute, _, invalidate, {Regexps, Pred}} | Rest], Tree, Annotations) ->
    transform_tree(Rest, Tree, [{cond_invalidator, Regexps, Pred} | Annotations]);
transform_tree([{attribute, _, invalidate, Regexps} | Rest], Tree, Annotations) ->
    transform_tree(Rest, Tree, [{invalidator, Regexps} | Annotations]);
transform_tree([{attribute, _, invalidate_groups, {Groups, Pred}} | Rest], Tree, Annotations) ->
    transform_tree(Rest, Tree, [{cond_group_invalidator, Groups, Pred} | Annotations]);
transform_tree([{attribute, _, invalidate_groups, Groups} | Rest], Tree, Annotations) ->
    transform_tree(Rest, Tree, [{group_invalidator, Groups} | Annotations]);
transform_tree([{attribute, _, module, Name} = A | Rest], Tree, Annotations) ->
    put(module_name, Name),
    transform_tree(Rest, [A | Tree], Annotations);
transform_tree([{function, _, _, _, _} = F | Rest], Tree, Annotations) ->
    Fun = transform_function(F, sort(Annotations)),
    transform_tree(Rest, [Fun | Tree], []);
transform_tree([Element | Rest], Tree, Annotations) ->
    transform_tree(Rest, [Element | Tree], Annotations);
transform_tree([], Tree, _) ->
    lists:reverse(Tree).

-spec(transform_function/2 :: (tuple(), list(tuple())) -> tuple()).
transform_function({function, Line, Name, Arity, Clauses0}, Annotations) ->
    Clauses = lists:map(fun(Clause) ->
				element(2, lists:foldl(fun transform_clause/2, {Name, Clause}, Annotations))
			end, Clauses0),

    {function, Line, Name, Arity, Clauses}.

-spec(transform_clause/2 :: (tuple(), list(tuple())) -> tuple()).	     
transform_clause(backend_call, {Name, C}) ->
    {Name, transform_backend_call(C, Name)};
transform_clause({invalidator, Regexps}, {Name, C}) ->
    {Name, transform_invalidator(C, Regexps)};
transform_clause({group_invalidator, Groups}, {Name, C}) ->
    {Name, transform_group_invalidator(C, Groups)};
transform_clause({cond_invalidator, Regexps, Pred}, {Name, C}) ->
    {Name, transform_invalidator(C, Regexps, Pred)};
transform_clause({cond_group_invalidator, Groups, Pred}, {Name, C}) ->
    {Name, transform_group_invalidator(C, Groups, Pred)}.

-spec(transform_backend_call/2 :: (tuple(), atom()) -> tuple()).	     
transform_backend_call({clause, Line, Args, Guards, Body}, FunName) ->
    FirstLine = element(2, hd(Body)),

    NodeVarName = get_unique_atom(),
    RpcErrorName = get_unique_atom(),
    RpcResult = get_unique_atom(),
    
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
 		      {var, FirstLine, NodeVarName}]
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
 		     [{var, FirstLine, NodeVarName},
 		      {atom, FirstLine, get(module_name)},
 		      {atom, FirstLine, FunName},
 		      prepare_arguments_list(Args, FirstLine)]
 		    },
 		    [{clause, FirstLine, 
 		      [{tuple, FirstLine, 
 			[{atom, FirstLine, badrpc},
 			 {var, FirstLine, RpcErrorName}]
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
 			   {var, FirstLine, RpcErrorName},
 			   {nil, FirstLine}
 			  }
 			 }]
 		       },
 		       {tuple, FirstLine, 
 			[{atom, FirstLine, badrpc},
 			 {var, FirstLine, RpcErrorName}]
 		       }]
 		     },
 		     {clause, FirstLine, 
 		      [{var, FirstLine, RpcResult}],
 		      [],
		      [{var, FirstLine, RpcResult}]
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
    transform_gen_invalidator(Clause, invalidate, get_unique_atom(), Regexps).

-spec(transform_group_invalidator/2 :: (tuple(), list(tuple())) -> tuple()).	
transform_group_invalidator(Clause, Groups) ->
    transform_gen_invalidator(Clause, invalidate_groups, get_unique_atom(), Groups).

-spec(transform_invalidator/3 :: (tuple(), list(tuple()), atom() | {atom(), atom()}) -> tuple()).	     
transform_invalidator(Clause, Regexps, Pred) ->
    transform_gen_invalidator(Clause, invalidate, get_unique_atom(), Regexps, Pred).

-spec(transform_group_invalidator/3 :: (tuple(), list(tuple()), atom() | {atom(), atom()}) -> tuple()).	
transform_group_invalidator(Clause, Groups, Pred) ->
    transform_gen_invalidator(Clause, invalidate_groups, get_unique_atom(), Groups, Pred).


-spec(transform_gen_invalidator/4 :: (tuple(), atom(), atom(), list(string())) -> tuple()).	     
transform_gen_invalidator({clause, Line, Args, Guards, Body}, Fun, Var, Regexps) ->
    FirstLine = element(2, hd(Body)),
    LastLine = element(2, hd(lists:reverse(Body))),

    NodeType = get_unique_atom(),
    
    NewBody = [{match, FirstLine, 
		{var, FirstLine, Var},
		{block, FirstLine, Body}
	       },
	       {'case', LastLine,
		{call, LastLine,
		 {remote, LastLine,
		  {atom, LastLine, application},
		  {atom, LastLine, get_env}
		 },
		 [{atom, LastLine, eptic},
		  {atom, LastLine, node_type}]
		},
		[{clause, LastLine,
		  [{tuple, LastLine, 
		    [{atom, LastLine, ok},
		     {var, LastLine, NodeType}]
		   }],
		  [[{op, LastLine, '==',
		     {var, LastLine, NodeType},
		     {atom, LastLine, backend}
		    }],
		   [{op, LastLine, '==',
		     {var, LastLine, NodeType},
		     {atom, LastLine, single_node_with_cache}
		    }]],
		  [{call, LastLine, 
		    {remote, LastLine,
		     {atom, LastLine, e_cluster},
		     {atom, LastLine, Fun}},
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

-spec(transform_gen_invalidator/5 :: (tuple(), atom(), atom(), list(string()), atom() | {atom(), atom()}) -> tuple()).
transform_gen_invalidator({clause, Line, Args, Guards, Body}, Fun, Var, Regexps, Pred) ->
    FirstLine = element(2, hd(Body)),
    LastLine = element(2, hd(lists:reverse(Body))),

    NodeType = get_unique_atom(),

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
		  [{'case', LastLine,
		    {call, LastLine,
		     {remote, LastLine,
		      {atom, LastLine, application},
		      {atom, LastLine, get_env}
		     },
		     [{atom, LastLine, eptic},
		      {atom, LastLine, node_type}]
		    },
		    [{clause, LastLine,
		      [{tuple, LastLine, 
			[{atom, LastLine, ok},
			 {var, LastLine, NodeType}]
		       }],
		      [[{op, LastLine, '==',
			 {var, LastLine, NodeType},
			 {atom, LastLine, backend}
			}],
		       [{op, LastLine, '==',
			 {var, LastLine, NodeType},
			 {atom, LastLine, single_node_with_cache}
			}]],
		      [{call, LastLine, 
			{remote, LastLine,
			 {atom, LastLine, e_cluster},
			 {atom, LastLine, Fun}},
			[prepare_list_of_strings(Regexps, LastLine)]
		       }]
		     },
		     {clause, LastLine, 
		      [{var, LastLine, '_'}],
		      [],
		      [{atom, LastLine, ok}]
		     }]
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

-spec(get_unique_atom/0 :: () -> atom()).	     
get_unique_atom() ->
    list_to_atom(lists:flatten(io_lib:format("~w", [now()]))).

-spec(sort/1 :: (list(atom() | tuple())) -> list(atom() | tuple())).
sort(Annotations) ->
    lists:sort(fun sort/2, Annotations).

-spec(sort/2 :: (atom() | tuple(), atom() | tuple()) -> bool()).
sort(backend_call, Else) when is_tuple(Else) ->
    true;
sort(Else, backend_call) when is_tuple(Else) ->
    false;
sort(_, _) ->
    true.
