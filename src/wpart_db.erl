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
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc Module responsible for transforming record entity representation to the format accepted by the form generator.
%%% Because the form generator expects the proplist of the fields
%%% to display, the record structure must be transformed to the new
%%% format. Moreover, wpart_db deals with the long name generation:
%%% if records are nested, then the paths to the particular fields must
%%% be unambiguous.<br/>
%%% The result of the runnning build_record_structure should be stored
%%% under <i>"__edit"</i> field in the request dictionary.
%%% @end
%%%-------------------------------------------------------------------

-module(wpart_db).
-export([build_record_structure/2, build_record_structure/3]).

-include_lib("stdlib/include/qlc.hrl").

%%
%% @spec build_record_structure(Name :: atom(), PrimaryKey :: integer()) -> FieldsProplist :: list(tuple())
%% @doc Rebuilds the list of the fields.
%% The record is read straight from the database - the <i>PrimaryKey</i>
%% is used for that reason.
%%
-spec(build_record_structure/2 :: (atom(), integer()) -> list(tuple())).	     
build_record_structure(Name, Number) when is_integer(Number), is_atom(Name) ->
    build_record_structure(Name, Number, atom_to_list(Name)).

-spec(find_primary_key/1 :: (list(tuple())) -> false | integer()).
find_primary_key([]) ->
    false;
find_primary_key([{_, Attrs} | Rest]) ->
    case lists:keymember(primary_key, 1, Attrs) of
	false -> find_primary_key(Rest);
	true -> length(Rest)
    end.

%%
%% @FIXME remove the mnesia read code - it is completely incompatible with the latest 
%% framework version
%% @spec build_record_structure(Name :: atom(), Type :: atom() | integer(), Record :: term()) -> list(tuple())
%% @doc Rebuilds the record structure.
%% If the <i>Type</i> parameter is set to <i>initial</i> the content
%%
-spec(build_record_structure/3 :: (atom(), atom() | integer(), term()) -> list(tuple())).	     
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
      
-spec(expand/2 :: (list(), list()) -> list(tuple())).	     
expand(List, Types) ->
    Zipped = lists:zip(Types, List),
    Checker = fun({{Type, _}, {Name, Val}}, Acc) ->
		      case lists:member(Type, e_conf:primitive_types()) of
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
