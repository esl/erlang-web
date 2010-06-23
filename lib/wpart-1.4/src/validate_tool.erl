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
%%% @author  : Michal Zajda <michal.zajda@erlang-consulting.com>
%%% Description : 
%%%
%%%-------------------------------------------------------------------
-module(validate_tool).
-export([validate_cu/2]).
-export([do_validate_ok/3, do_validate_ok/4, 
	 do_validate_error/4, do_validate_error/5]).

%% @doc Create Update validation, retrieves all fields declared in record.
%% WARNING: sets primarykey to undefined! not -1.
-spec(validate_cu/2 :: (atom(), atom()) -> {ok, tuple()} | {error, not_valid | {atom(), integer()}}).
validate_cu(Type, Fun) ->
    Mod = list_to_atom("wtype_" ++ atom_to_list(Type)),

    Funs = Mod:module_info(exports),
    ParentType = case lists:member({get_parent_type,0},Funs) of
		     true ->
			 Mod:get_parent_type();
		     _ ->
			 Type
		 end,    

    case wpart_valid:validate(Type) of
        {ok, List} ->
	    do_validate_ok(List,Type,Mod,ParentType);
	{error, Reason} ->
            do_validate_error(Mod,Reason,Fun,Type,ParentType)
    end.

-spec(do_validate_ok/3 :: (list(), atom(), atom()) -> {ok, tuple()}).
do_validate_ok(List,Mod,Type) ->
    do_validate_ok(List, Type, Mod, Type).

-spec(do_validate_ok/4 :: (list(), atom(), atom(), atom()) -> {ok, tuple()}).     
do_validate_ok(List,Type,Mod,ParentType) ->
    Fields = Mod:get_record_info(Type),
    
    {Result, _Bad} = get_values(Type, Fields, List),

    %% special case of primary key, if it's not in post - it's creating not editing
    Pr = get_primary_key(Mod, Type),
    Final = replace_primary(Result, Pr),

    {ok, list_to_tuple([ParentType | Final])}.

-spec(do_validate_error/4 :: (atom(), term(), atom(), atom()) -> 
	     {error, not_valid | {term(), term()}}).
do_validate_error(Mod,Reason,Fun,Type) ->
    do_validate_error(Mod,Reason, Fun, Type, Type).

-spec(do_validate_error/5 :: (atom(), term(), atom(), atom(), atom()) -> 
	     {error, not_valid | {atom(), integer()}}). 
do_validate_error(Mod,Reason,Fun,Type,ParentType) ->
    Fields = apply(Mod, get_record_info, [Type]),

    {Not_ValidatedValues, Reason_list} = get_values(Type, Fields, Reason),
    
    wpart:fset("__not_validated", list_to_tuple([ParentType | Not_ValidatedValues])),
    wpart:fset("__error", string:join(Reason_list, ", ")),
    
    %% special case of primary key - if error on edit returns {ok,[]} to get original 
    %% values from DB or set special initial values.
    case get_primary_key(Mod, Type)  of
	undefined -> {error, not_valid};
	{_, Term} -> {error, {Fun, Term}}
    end.

-spec(replace_primary/2 :: (list(), undefined | {integer(), term()}) -> list()).
replace_primary(Result, undefined) ->
    Result;
replace_primary(Result, {PKPos, PK}) ->
    {Front, [_ | Back]} = lists:split(PKPos-1, Result),
    lists:append([Front, [PK | Back]]).
    
-spec(get_values/3 :: (atom(), list(), list()) -> {list(string()), list(string())}).	     
get_values(Type,Fields,List) ->
    TypeS = atom_to_list(Type),
    post_get(TypeS,Fields,List,[],[]).

-spec(post_get/5 :: (string(), list(), list(), list(string()), list(string())) -> {list(string()), list(string())}).	     
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
			   Reason_list ++ [io_lib:format("~p", [Reason])]);
	       true ->
                  post_get(TypeS,Next,List,R ++ [Input],
			   Reason_list ++ [io_lib:format("~p", [Reason])])
	    end
   end.

-spec(get_primary_key/2 :: (atom(), atom()) -> undefined | {integer(), term()}). 
get_primary_key(Mod, Type) ->
    case eptic:fget("post", "__primary_key") of
	undefined -> 
	    undefined;
	PK ->
	    TypeOpts = tl(tuple_to_list(Mod:get_record_info(
					  list_to_atom(atom_to_list(Type) ++ "_types")))),
	    {PKPos, {PKType, PKOpts}} = case wpart_utils:find_pk(TypeOpts) of
					    no_pk ->
						{1, hd(TypeOpts)};
					    Else ->
						Else
					end,

	    case wpart_utils:string2term(PKType, PK, PKOpts) of
		{ok, PKTerm} ->
		    {PKPos, PKTerm};
		_ ->
		    undefined
		 end
    end.
    
