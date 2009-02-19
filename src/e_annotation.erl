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
%% <li><i>-invalidate({Fun/Arity, [Regexp]})</i> - defines, that
%% right after the function execution, the invalidation of the 
%% regexps should be processed (always! - without inspecting the
%% result of the function). The called function is 
%% e_cluster:invalidate/1</li>
%% <li><i>-invalidate_groups({Fun/Arity, [Group]})</i> - the same
%% as <i>invalidate</i> but invalidating groups instead of ids.</li>
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
transform_function1(F, [{backend_call, _} | Rest]) ->
    transform_function1(transform_backend_call(F), Rest);
transform_function1({function, Line, Name, Arity, Clauses0}, [{invalidator, _, Regexps} | Rest]) ->
    Clauses = lists:map(fun(Clause) ->
				transform_invalidator(Clause, Regexps)
			end, Clauses0),

    transform_function1({function, Line, Name, Arity, Clauses}, Rest);
transform_function1(F, []) ->
    F.

-spec(transform_backend_call/1 :: (tuple()) -> tuple()).	     
transform_backend_call({function, _Line, _Name, _Arity, _Clauses} = F) ->
    F.

-spec(transform_invalidator/2 :: (tuple(), list(string())) -> tuple()).	     
transform_invalidator({clause, Line, Args, Guards, Body}, Regexps) ->
    FirstLine = element(2, hd(Body)),
    LastLine = element(2, hd(lists:reverse(Body))),
    
    NewBody = [{match, FirstLine, 
		{var, FirstLine, ?INVALIDATOR_VAR_NAME},
		{block, FirstLine, Body}
	       },
	       {call, LastLine, 
		{remote, LastLine,
		 {atom, LastLine, e_cluster},
		 {atom, LastLine, invalidate}},
		[prepare_list_of_strings(Regexps, LastLine)]
	       },
	       {var, LastLine, ?INVALIDATOR_VAR_NAME}],

    {clause, Line, Args, Guards, NewBody}.

-spec(prepare_list_of_strings/2 :: (list(string()), integer()) -> tuple()).	     
prepare_list_of_strings([H | R], Line) ->
    {cons, Line, 
     {string, Line, H},
     prepare_list_of_strings(R, Line)};
prepare_list_of_strings([], Line) ->
    {nil, Line}.
