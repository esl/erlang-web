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
%%% Description : 
%%%
%%%-------------------------------------------------------------------
-module(wpart_valid).
-export([validate/1, validate/2, validate/3, is_private/1]).

part(0,_,_,_,BasicFields,ComplexTypes, BasicTypes) ->
    {BasicFields, ComplexTypes, BasicTypes};
part(N, Fields, TypesList, BTList, BasicFields, ComplexTypes, BasicTypes) -> 
    [H1|T1] = TypesList,
    [H2|T2] = Fields,
    {X,_} = H1,
    Res = lists:member(X, BTList),
    if Res -> part(N-1,T2,T1,BTList,
		   [H2 | BasicFields],
		   ComplexTypes, [H1 | BasicTypes]);
       true -> part(N-1,T2,T1,BTList,BasicFields,[H1 | ComplexTypes], BasicTypes)
    end.

%% From : ["news","person"]
validate(Fields, Types, From) ->
    TypesList = tuple_to_list(Types),
    [_ | TypesListTail] = TypesList,

    {BasicFields, Complex, Basic} = part(length(TypesList)-1, Fields, 
					 TypesListTail, e_conf:primitive_types(),[],[],[]),
    LongBasicFields = lists:map(fun(X)-> string:join(From, "_") ++ 
					     "_" ++ atom_to_list(X) end, 
				BasicFields),

    LeafResult = if 
		     Complex =/= [] -> 
			 [[_ | Val]] = lists:map(fun({X,_}) -> 
							 validate(X, From) 
						 end,
						 Complex),
			 Val;
		     true -> []
		 end,

    Fun = fun({{collection, _}, X}) -> 
		  get_collection_fields(X);
	     ({_, X}) ->
		  eptic:fget("post", X)
	  end,
    PostInput = lists:map(Fun, lists:zip(Basic, LongBasicFields)),

    Final = tuple_to_list(pusher(PostInput, Basic, LongBasicFields)),

    Final ++ LeafResult.

validate(Name) when is_atom(Name) ->
    [_ | R] = apply(list_to_atom("wtype_" ++ atom_to_list(Name)), 
			 validate, [[]]),
    Result = lists:flatten(R),
    Temp = lists:map(fun({{X,_},_})-> X end, Result),
    
    Bad = lists:member(error, Temp),
    if not Bad -> {ok, Result};
       Bad -> {error, Result}
    end.
 
validate(Name, From) when is_atom(Name) ->
    apply(list_to_atom("wtype_" ++ atom_to_list(Name)), validate, [From]). 

%% BasicType - {date, [{format, YYYY-MM-DD}]}
pusher(Input, BasicTypes, LongBasicFields) ->
    Pairs = lists:zip(BasicTypes, Input),
    Result = lists:map(fun check/1, Pairs),
    save_errors(LongBasicFields, Result),
    Bad = lists:keymember(error,1,Result),
    if   
	not Bad -> {ok, show_output(Result,LongBasicFields,[])};
	true -> {error, show_output(Result,LongBasicFields,[])}
    end.

show_output([],_,R) ->
    R;
show_output(Result,LongBasicFields, Output) ->
   [H|T] = Result,
   [HH|TT] = LongBasicFields,  
   show_output(T,TT, Output ++ [{H, HH}]).

%% helper function to check if input has good type (using called wtype_ )
%% takes structure eqivalent to types record and input string 
%% check ( { {date, [{format, YYYY-MM-DD}]} , input_string } )
%% returns proper input (if oryginal one was not complete) {ok, StrOk} or {error, Msg}
check({Type, Input}) ->
    {H,T} = Type, 
    apply(list_to_atom("wtype_" ++ atom_to_list(H)),validate,[{T,Input}]).

%% function which checks if the field should be validated
%% if it is a private field - not shown on forms - it will be skipped
%% beacuse it will be set by some default value later (e.g. ID)
is_private(Params) ->
    case lists:keysearch(private, 1, Params) of
	{value, {private, Val}} ->
	    Val;
	false -> 
	    false
    end.

get_collection_fields(Name) ->
    lists:foldl(fun({Key, Val}, Acc) ->
			case string:str(Key, Name) of
			    0 ->
				Acc;
			    _ ->
				[{Key, Val} | Acc]
			end
		end, [], eptic:fget("post")).

-spec(save_errors/2 :: (list(string()), list()) -> ok).	     
save_errors([], []) ->
    ok;
save_errors([FieldName | RestFields], [{error, Reason} | RestResults]) ->
    e_error:save(FieldName, Reason),
    save_errors(RestFields, RestResults);
save_errors([_ | RestFields], [_ | RestResults]) ->
    save_errors(RestFields, RestResults).
