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
%%% File    : wpart_db.erl
%%% Author  : Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% Description : 
%%%
%%%-------------------------------------------------------------------
-module(wpart_db).
-export([build_record_structure/2,build_record_structure/3]).

-include_lib("stdlib/include/qlc.hrl").

-deprecated([build_record_structure/3]).

-spec(build_record_structure/2 :: (atom(), tuple()) -> list({string(), term()})).	     
build_record_structure(TypeName, TypeValues) ->
    build_record_structure0(TypeName, TypeValues, atom_to_list(TypeName) ++ "_").

-spec(build_record_structure/3 :: (atom(), term(), tuple()) -> list({string(), term()})).
build_record_structure(TypeName, _, TypeValues) ->	     
    build_record_structure0(TypeName, TypeValues, atom_to_list(TypeName) ++ "_").

-spec(build_record_structure0/3 :: (atom(), tuple(), string()) -> list({string(), term()})).	     
build_record_structure0(TypeName, TypeValues0, Prefix) ->
    NameString = atom_to_list(TypeName),
    TypeDefs0 = (list_to_atom("wtype_" ++ NameString)):get_record_info(
		  list_to_atom(NameString ++ "_types")),
    TypeDefs = tl(tuple_to_list(TypeDefs0)),
    TypeNames = (list_to_atom("wtype_" ++ NameString)):get_record_info(TypeName),
    TypeValues = tl(tuple_to_list(TypeValues0)),

    Primitives = e_conf:primitive_types(),
    
    lists:flatten(lists:foldl(fun({Val, {Name, {Type, _}}}, Acc) ->
				      case lists:member(Type, Primitives) of
					  true ->
					      [{Prefix ++ atom_to_list(Name), Val} | Acc];
					  false ->
					      [build_record_structure(Type, Val, 
								      Prefix ++ atom_to_list(Type) ++ "_") |
					       Acc]
				      end
			      end,
			      [], lists:zip(TypeValues, lists:zip(TypeNames, TypeDefs)))).
