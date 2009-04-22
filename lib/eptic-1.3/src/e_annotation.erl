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
%%% File    : e_annotation.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc Main engine for the annotation language extension.
%%% It allows users to create their own annotations: transforms the 
%%% selected functions (either marked as ?BEFORE or ?AFTER) into 
%%% annotations and creates the proper header file (in the include
%%% directory of the application containing the annotation).
%%% The user-defined annotations are processed by <i>e_user_annotation</i>
%%% module.
%%% @end
%%% Created : 22 Apr 2009 by Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%%-------------------------------------------------------------------
-module(e_annotation).

-export([parse_transform/2]).

parse_transform(Tree, _Options) ->
    put(ew_annotations, []),
    transform_tree(Tree, [], none).

transform_tree([{attribute, _, ew_annotation_before, _} | Rest], Tree, _) ->
    transform_tree(Rest, Tree, before);
transform_tree([{attribute, _, ew_annotation_after, _} | Rest], Tree, _) ->
    transform_tree(Rest, Tree, 'after');
transform_tree([{attribute, _, module, Name} = A | Rest], Tree, AnnotationType) ->
    put(module_name, Name),
    transform_tree(Rest, [A | Tree], AnnotationType);
transform_tree([{attribute, _, file, {Path, _}} = A | Rest], Tree, _) ->
    put(module_path, Path),
    transform_tree(Rest, [A | Tree], none);
transform_tree([{function, _, _, _, _} = F | Rest], Tree, none) ->
    transform_tree(Rest, [F | Tree], none);
transform_tree([{function, _, _, _, _} = F | Rest], Tree, AnnotationType) ->
    NewF = transform_function(F, AnnotationType),
    transform_tree(Rest, [NewF | Tree], none);
transform_tree([Element | Rest], Tree, AnnotationType) ->
    transform_tree(Rest, [Element | Tree], AnnotationType);
transform_tree([], Tree, _) ->
    HrlName = filename:join([filename:dirname(get(module_path)), "..", "include", 
			     atom_to_list(get(module_name)) ++ "_annotations.hrl"]),
    case file:open(HrlName, [write]) of
	{ok, Fd} ->
	    io:format(Fd, "-compile({parse_transform, e_user_annotation}).~n"
		      "-compile(nowarn_shadow_vars).~n~n", []),
	    save_annotations(Fd, lists:reverse(get(ew_annotations)));
	{error, Reason} ->
	    io:format("Error during annotation header creation: ~p. Reason: ~p~n",
		      [HrlName, Reason])
    end,
    lists:reverse(Tree).

transform_function({function, _, FunName, 4, _} = Fun, Type) ->
    put(ew_annotations, [{Type, get(module_name), FunName} | get(ew_annotations)]),
    Fun;
transform_function({function, LineNo, FunName, _, _} = Fun, Type) ->
    io:format("~p.erl:~p: function ~p must be of arity 4, skipping ~p annotation~n", 
	      [get(module_name), LineNo, FunName, Type]),
    Fun.
    
save_annotations(Hrl, [{Type, ModName, FunName} | Rest]) ->
    io:format(Hrl, "-define(~s(Args), -ew_user_annotation({Args, ~p, ~p, ~p})).~n~n",
	      [generate_define_name(FunName), Type, ModName, FunName]),
    save_annotations(Hrl, Rest);
save_annotations(Hrl, []) ->
    file:close(Hrl).

-spec(generate_define_name/1 :: (atom()) -> (string())).	     
generate_define_name(FunName) ->
    string:to_upper(atom_to_list(FunName)).
