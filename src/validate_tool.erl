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
%%% File    : validate_tool.erl
%%% Author  : Michal Zajda <michal.zajda@erlang-consulting.com>
%%% Description : 
%%%
%%%-------------------------------------------------------------------

-module(validate_tool).
-export([validate_cu/2]).
-export([replace_primary/4, do_validate_ok/3, do_validate_ok/4, 
	 do_validate_error/4, do_validate_error/5]).

%% @edoc Create Update validation, retrieves all fields declared in record.
%% WARNING: sets primarykey to undefined! not -1.
validate_cu(Type, Fun) ->
    Mod = list_to_atom("wtype_" ++ atom_to_list(Type)),

    Funs = Mod:module_info(exports),
    ParentType = case lists:member({get_parent_type,0},Funs) of
		     true ->
			 apply(Mod, get_parent_type, []);
		     _ ->
			 Type
		 end,    

    case wpart_valid:validate(Type) of
        {ok, List} ->
	    do_validate_ok(List,Type,Mod,ParentType);
	{error, Reason} ->
            do_validate_error(Mod,Reason,Fun,Type,ParentType)
    end.


do_validate_ok(List,Mod,Type) ->
    do_validate_ok(List,Type, Mod,Type).

do_validate_ok(List,Type,Mod,ParentType) ->
    Fields = apply(Mod, get_record_info, [Type]),

    {Result, _Bad} = get_values(Type, Fields, List),

    %% special case of primary key, if it's not in post - it's creating not editing
    Pr = case eptic:fget("post", "__primary_key") of
	     undefined -> undefined;
	     P -> list_to_integer(P)
	 end,
    Final = replace_primary(Result,Pr,Mod,Type),
    FinalTuple = list_to_tuple([ParentType] ++ Final),
    {ok, FinalTuple}.

do_validate_error(Mod,Reason,Fun,Type) ->
    do_validate_error(Mod,Reason, Fun, Type, Type).

do_validate_error(Mod,Reason,Fun,Type,ParentType) ->
    Fields = apply(Mod, get_record_info, [Type]),

    {Not_ValidatedValues, Reason_list} = get_values(Type, Fields, Reason),
    
    wpart:fset("__not_validated", list_to_tuple([ParentType]++ Not_ValidatedValues)),
    
    wpart:fset("__error", string:join(Reason_list, ", ")),
    
    %% special case of primary key - if error on edit returns {ok,[]} to get oryginal 
    %% values from DB or set special initial values.
    case eptic:fget("post", "__primary_key") of
	undefined -> {error, not_valid};
	P -> {error, {Fun, list_to_integer(P)}}
    end.

replace_primary(Result,undefined,_Mod,_Type) ->
    Result;
replace_primary(Result,Pr,Mod,Type) ->
     Arg = list_to_atom(atom_to_list(Type) ++ "_types"),
     [_Name | Rest] = tuple_to_list(apply(Mod, get_record_info, [Arg])),
     {R,No} = find_primary(Rest,1,any),
     Final = if R =/= ok -> {error, no_primary_key};
        true ->
            replace_elem(Result,Pr,No)
     end,
     Final.

replace_elem(Result,Pr,No) ->
   {Front, Back} = lists:split((No - 1),Result),
   [_H|T] = Back,
   NewBack = [Pr|T],
   Front ++ NewBack.

find_primary([],No,ok) ->
    {ok,No};
find_primary([],No,false)->
    {false, No};
find_primary(Rest,No,_OK) ->
    [{_Type, Options}|T] = Rest,
    X = lists:keysearch(primary_key,1,Options),
    if X == {value, {primary_key}} -> find_primary([],No,ok);
       true -> find_primary(T,No+1,false)
    end.

get_values(Type,Fields,List) ->
    TypeS = atom_to_list(Type),
    post_get(TypeS,Fields,List,[],[]).

post_get(_Type,[],_List,R,Reason_list) ->
    {R,Reason_list};

%% TODO: refactor
post_get(TypeS,[H|Next],List,R,Reason_list) ->

   Field = atom_to_list(H),
   LongName = TypeS ++ "_" ++ Field,
    
   {value, {{Bla, X}, LongName}} = lists:keysearch(LongName, 2, List),
   if 
        Bla == ok -> 
            post_get(TypeS,Next,List,R ++ [X],Reason_list);
	true -> 
	    wpart:fset(LongName, "failed"), 
	    {Reason, Input} = X, 
	    if Input == undefined -> 
                  post_get(TypeS,Next,List,R ++ [""],
			   Reason_list ++ [atom_to_list(Reason)]);
	       true ->
                  post_get(TypeS,Next,List,R ++ [Input],
			   Reason_list ++ [atom_to_list(Reason)])
	    end
   end.

   
