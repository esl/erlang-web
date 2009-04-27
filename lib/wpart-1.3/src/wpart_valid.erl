-module(wpart_valid).
-export([validate/1, is_private/1]).


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
validate(_, _, [[]], Acc, ErrorCount) ->
    {ErrorCount,Acc};
%% clause to match call when POST is empty (form building)
validate(undefined, Primitives, [{Predessor, TypeName} | MoreTypes], Acc, ErrorCount) ->
    validate([], Primitives, [{Predessor, TypeName} | MoreTypes], Acc, ErrorCount);
validate(POST, Primitives, [{Predessor, TypeName} | MoreTypes], Acc, ErrorCount) ->

    %% {[atom()],tuple()}
    {RecInfo, TypeRecInstance} = get_record_info(TypeName),

    [_ | Types] = tuple_to_list(TypeRecInstance),

    {groups, PostNames, BasicTypes, ComplexFields} = 
	sort_out(RecInfo,Types, Primitives, Predessor),
    
    Input = zip_get(BasicTypes, PostNames, POST, []),
    
    Result = validate_local(Input, BasicTypes),
    
    %% posible to optimize and combine with validate_local
    Errors = save_errors(PostNames, Result),
    ResultExt = lists:zip(Result,PostNames),

    validate(POST, Primitives, [ComplexFields|MoreTypes], 
	     ResultExt ++ Acc, ErrorCount + Errors).

validate_local(L1,L2) ->
    validate_local(L1,L2,[]).

validate_local([],[],Acc) ->
    Acc;
validate_local([In|InputTail], [{Name,Attr} | BasicTypesTail], Acc) ->
    Result = apply(list_to_atom("wtype_" ++ atom_to_list(Name)),validate,[{Attr,In}]),
    validate_local(InputTail,BasicTypesTail,[Result|Acc]).

%% @doc function retrieves form post single value or set of values.
%% It should not be unified because only collection fields could have multipule vals under one key;
%% more than one val in one-POST-er field is hack or bug and should not be propagated.
zip_get([],[],_,Acc)->
    Acc;
zip_get([{collection,_}|T1],[H2|T2], POST, Acc) ->    
    In = proplists:get_all_values(H2,POST),
    zip_get(T1,T2,POST,[In|Acc]);
zip_get([_|T1],[H2|T2], POST, Acc) ->
    In = proplists:get_value(H2,POST),
    zip_get(T1,T2,POST,[In|Acc]).

%% @doc divides fields into groups to discover which fields' names are ready to retrieve from POST.
%% It is not obvious, because types can be nested and have hierarchical names.
%% Basic types fields are resolved to proper names (predessor name added with '_').
%% To complex type current type is passed to become predessor.
sort_out(RecInfo, Types, Primitives, Before) ->
    sort_out(RecInfo, Types, Primitives, Before,[],[],[]).

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


%-spec(save_errors/2 :: (list(string()), list()) -> ok).	     
save_errors(L1,L2) ->
    save_errors(L1,L2,0).

save_errors([], [], Err) ->
    Err;
save_errors([FieldName | RestFields], [{error, Reason} | RestResults], Err) ->
    e_error:save(FieldName, Reason),
    save_errors(RestFields, RestResults, Err+1);
save_errors([_ | RestFields], [_ | RestResults], Err) ->
    save_errors(RestFields, RestResults, Err).

%% Private fields are not displayed and validated
is_private(Params) ->
    case lists:keysearch(private, 1, Params) of
	{value, {private, Val}} ->
	    Val;
	false -> 
	    false
    end.
