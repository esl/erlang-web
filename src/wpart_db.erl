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
%-export([save/3]).

-include_lib("stdlib/include/qlc.hrl").

build_record_structure(Name, Number) when is_integer(Number)
					  andalso is_atom(Name) ->
    build_record_structure(Name, Number, atom_to_list(Name)).

find_primary_key([]) ->
    false;
find_primary_key([{_, Attrs} | Rest]) ->
    case lists:keymember(primary_key, 1, Attrs) of
	false -> find_primary_key(Rest);
	true -> length(Rest)
    end.

build_record_structure(Name, initial, Explicite) when is_atom(Name) ->
    eptic:fset("initial", Explicite),
    build_record_structure(Name, notused, atom_to_list(Name));

build_record_structure(Name, Number, Prefix) ->
    NameString = atom_to_list(Name),

    Module = list_to_atom("wtype_" ++ NameString),
    [_ | Types] = tuple_to_list(apply(Module, get_record_info, 
				      [list_to_atom(NameString ++ 
						    "_types")])),

    PrimaryKeyPosition = length(Types) - find_primary_key(Types) + 1,

    Fun = fun() ->
		  qlc:e(
		    qlc:q(
		      [X
		       || X <- mnesia:table(Name), 
			  lists:nth(PrimaryKeyPosition, 
				    tuple_to_list(X)) == Number]
		     ))
	  end,
    
    {atomic, [Rcrd]} = if 
			   Number =/= notused ->
			       mnesia:transaction(Fun);
			   true -> 
			       {atomic, [eptic:fget("initial")]} 
		       end,
    
    Record = tl(tuple_to_list(Rcrd)),

    Mod = list_to_atom("wtype_"++NameString),
    Attr = Mod:get_record_info(Name),
    Names = lists:map(fun(X) -> 
%%			      list_to_atom(Prefix ++ "_" ++ 
%%					   atom_to_list(X))
			      Prefix ++ "_" ++ atom_to_list(X)
		      end, Attr),
    Zipped = lists:zip(Names, Record),

    expand(Zipped, Types).
      
expand(List, Types) ->
    [PrimitivesT] = ets:tab2list(basic_types),
    Primitives = tuple_to_list(PrimitivesT) ++ e_conf:primitive_types(),

    Zipped = lists:zip(Types, List),
    Checker = fun({{Type, _}, {Name, Val}}, Acc) ->
		      case lists:member(Type, Primitives) of
			  true ->
			      [{Name, Val} | Acc];
			  false ->
			      [build_record_structure(Type, Val, 
						      atom_to_list(Name)) 
			       | Acc]
		      end
	      end,
    lists:flatten(lists:foldl(Checker, [], Zipped)).

%% remove_prefixes(Name, List) ->
%%     PrefixLength = length(Name) + 2,
%%     Remover = fun({{ok, Val}, X}) ->
%% 		      {list_to_atom(string:substr(X, PrefixLength)),
%% 		       Val}
%% 	      end,
%%     lists:map(Remover, List).

%% get_primary_key_name(Record) ->
%%     Module = list_to_atom("wtype_" ++ Record),
%%     [_ | Info] = tuple_to_list(apply(Module, get_record_info, 
%% 				     [list_to_atom(Record ++ "_types")])),
    
%%     Pos = length(Info) - find_primary_key(Info),
%%     lists:nth(Pos, Info).

%save(Name, PrefixedFields, UserSaveFuns) ->
%    NameString = atom_to_list(Name),
%    FFields = remove_prefixes(NameString, PrefixedFields).
    
%    Fields = case get_primary_key_name(NameString) of
%		 false -> [
%    io:format("Primarykey=~p~n", [PrimaryKey]),
%    io:format("FFields=~p~n", [FFields]).
