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
%%% File    : wpart_valid.erl
%%% Author  : Michal Zajda <michal.zajda@erlang-consulting.com>
%%% Description : Validation of custom types by dividing them into basic types 
%%%               and routing to the validate function in proper wtypes
%%%-------------------------------------------------------------------

-module(wpart_valid).
-export([validate/1, validate/3, is_private/1]).


%% @doc the initial call from a controller
-spec(validate/1 :: (atom()) -> {ok, list()} | {error, list()}).
validate(TypeName) ->
    POST = eptic:fget("post"),
    Primitives =  e_conf:primitive_types(),

    {ErrorCount, V_Result} = validate(POST, Primitives, 
				      [{atom_to_list(TypeName), TypeName}], [], 0),
     
    if   
	ErrorCount > 0 -> {error,V_Result};
	true -> {ok, V_Result}
    end.

%% @doc Function validates field by field from a record definition, when field aprears 
%% to be nested type its predessor is formed and passed with TypeName
-spec(validate/5 ::  (undefined|list(), list(), list(tuple()), list(), integer()) ->
	     {integer(),list(tuple())}).
validate(_, _, [[]], Acc, ErrorCount) ->
    {ErrorCount,Acc};
%% clause to match call when POST is empty (form building)
validate(undefined, Primitives, [{Predecessor, TypeName} | MoreTypes], Acc, ErrorCount) ->
    validate([], Primitives, [{Predecessor, TypeName} | MoreTypes], Acc, ErrorCount);
validate(POST, Primitives, [{Predecessor, TypeName} | MoreTypes], Acc, ErrorCount) ->

    %% {[atom()],tuple()}
    {RecInfo, TypeRecInstance} = get_record_info(TypeName),

    [_ | Types] = tuple_to_list(TypeRecInstance),

    {groups, PostNames, BasicTypes, ComplexFields} = 
	sort_out(RecInfo,Types, Primitives, Predecessor),
    
    Input = zip_get(BasicTypes, PostNames, POST, []),
    
    Result0 = validate_local(Input, BasicTypes),
    Result = validate_uniqueness(list_to_atom(Predecessor), Result0, BasicTypes),
    %% posible to optimize and combine with validate_local
    Errors = save_errors(PostNames, Result),
    ResultExt = lists:zip(Result,PostNames),

    validate(POST, Primitives, [ComplexFields|MoreTypes], 
	     ResultExt ++ Acc, ErrorCount + Errors).

-spec(validate_local/2 :: (list(),list()) -> list(tuple())).
validate_local(L1,L2) ->
    validate_local(L1,L2,[]).

-spec(validate_local/3 :: (list(),list(),list()) -> list(tuple())).
validate_local([],[],Acc) ->
    lists:reverse(Acc);
validate_local([In|InputTail], [{Name,Attr} | BasicTypesTail], Acc) ->
    Result = apply(list_to_atom("wtype_" ++ atom_to_list(Name)),validate,[{Attr,In}]),
    validate_local(InputTail,BasicTypesTail,[Result|Acc]).

-spec(validate_uniqueness/3 :: (atom(), list(), list()) -> list()).	     
validate_uniqueness(MasterType, Input, Types) ->
    validate_uniqueness(MasterType, Input, Types, [], length(Input)).

-spec(validate_uniqueness/5 :: (atom(), list(), list(), list(), integer()) -> 
	     list({ok, term()} | {error, term()})).
validate_uniqueness(Type, [{error, _} = Input | TInput], [_ | TType], Acc, Pos) ->
    validate_uniqueness(Type, TInput, TType, [Input | Acc], Pos-1);
validate_uniqueness(Type, [{ok, Input} | TInput], [{_, Attr} | TType], Acc, Pos) ->
    case lists:member(unique, Attr) of
	false ->
	    validate_uniqueness(Type, TInput, TType, [{ok, Input} | Acc], Pos-1);
	true ->
	    validate_uniqueness(Type, TInput, TType, 
				[check_uniqueness(Type, Input, Pos+1) | Acc], Pos-1)
    end;
validate_uniqueness(_, [], [], Acc, _) ->
    lists:reverse(Acc).

-spec(check_uniqueness/3 :: (atom(), term(), integer()) -> 
	     {ok, term()} | {error, term()}).	     
check_uniqueness(MasterType, Input, Pos) ->
    Entries = e_db:read(MasterType),
    Pred = fun(Element) when element(Pos, Element) == Input ->
		   true;
	      (_) ->
		   false
	   end,

    case lists:any(Pred, Entries) of
	true ->
	    {error, {element_not_unique, Input}};
	false ->
	    {ok, Input}
    end.

%% @doc function retrieves form post single value or set of values.
%% It should not be unified because only collection fields could have multipule vals under one key;
%% more than one val in one-POST-er field is hack or bug and should not be propagated.
-spec(zip_get/4 :: (list(tuple()), list(string()), 
		    list(tuple()), list(tuple())) -> list(tuple())).
zip_get([],[],_,Acc)->
    lists:reverse(Acc);
zip_get([{collection, _}|T1],[H2|T2], POST, Acc) ->
    In = proplists:get_all_values(H2,POST),
    zip_get(T1,T2,POST,[In|Acc]);
zip_get([{password, _}|T1], [H2|T2], POST, Acc) ->
    case proplists:get_all_values(H2, POST) of
	[_, _] = Val ->
	    zip_get(T1, T2, POST, [Val|Acc]);
	[Val] ->
	    zip_get(T1, T2, POST, [Val|Acc]);
	_ ->
	    zip_get(T1, T2, POST, [undefined|Acc])
    end;
zip_get([_|T1],[H2|T2], POST, Acc) ->
    In = proplists:get_value(H2,POST),
    zip_get(T1,T2,POST,[In|Acc]).

%% @doc divides fields into groups to discover which fields' names are ready to retrieve from POST.
%% It is not obvious, because types can be nested and have hierarchical names.
%% Basic types fields are resolved to proper names (predessor name added with '_').
%% To complex type current type is passed to become predessor.
-spec(sort_out/4 :: (list(atom()), list(tuple()), list(), string()) ->
	    	    {groups, list(string()), list(tuple()), list(tuple())}).
sort_out(RecInfo, Types, Primitives, Before) ->
    sort_out(RecInfo, Types, Primitives, Before,[],[],[]).

-spec(sort_out/7 :: (list(atom()), list(tuple()), list(), string(), list(), list(), list()) ->
	    	    {groups, list(string()), list(tuple()), list(tuple())}).
sort_out([], _, _, _,
	 BasicFields, BasicTypes, ComplexFields) ->
    {groups,  BasicFields, BasicTypes, ComplexFields};
sort_out([H1|T1], [H2={Type, _AttrList}|T2], Primitives, Before,
	 BasicFields, BasicTypes, ComplexFields) ->
    case lists:member(Type, Primitives) of
	true ->
	    sort_out(T1,T2,Primitives,Before,
		     [string:join([Before,atom_to_list(H1)],"_")|BasicFields],
		     [H2|BasicTypes],ComplexFields);
	false ->
	    sort_out(T1,T2,Primitives,Before,
		     BasicFields,BasicTypes,
		     [{string:join([Before,atom_to_list(H1)],"_"), H1}|ComplexFields])
    end.

%% @doc calls a custom wtype modules to get a custom type definition

-spec(get_record_info/1 :: (atom()) -> {list(atom()), list(tuple())}).
get_record_info(TypeName) ->
    %% TypeNameStr can be different from Predessor
    TypeNameStr = atom_to_list(TypeName),
    Module = list_to_atom("wtype_" ++ TypeNameStr),
    RecordInfo = apply(Module,
		       get_record_info,
		       [TypeName]),
    RecName = list_to_atom(TypeNameStr ++ "_types"),
    TypeRecordInstance = apply(Module,
			       get_record_info,
			       [RecName]),
    {RecordInfo, TypeRecordInstance}.


-spec(save_errors/2 :: (list(string()), list()) -> integer()).	     
save_errors(L1,L2) ->
    save_errors(L1,L2,0).

-spec(save_errors/3 :: (list(string()), list(), integer()) -> integer()).	
save_errors([], [], Err) ->
    Err;
save_errors([FieldName | RestFields], [{error, Reason} | RestResults], Err) ->
    e_error:save(FieldName, Reason),
    save_errors(RestFields, RestResults, Err+1);
save_errors([_ | RestFields], [_ | RestResults], Err) ->
    save_errors(RestFields, RestResults, Err).

%% Private fields are not displayed and validated
-spec(is_private/1 :: (list(tuple())) -> ( term() | false )).
is_private(Params) ->
    case lists:keysearch(private, 1, Params) of
	{value, {private, Val}} ->
	    Val;
	false -> 
	    false
    end.


%% @doc back compatibility function.
validate(_,_,_) ->
    depricated.
