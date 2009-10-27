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
%%% @author Michal Zajda <info@erlang-consulting.com>
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(wtype_csv).
-behaviour(wtype).

-export([handle_call/2, validate/1]).

handle_call(_, Val) ->
    Val.

validate({Options,undefined}) -> 
    case wpart_valid:is_private(Options) of
	true ->
	    {ok, undefined};
        false ->
            case lists:keysearch(optional, 1, Options) of
	       {value, {optional, Default}} -> 
                    {ok, Default};
	       _ ->  
                    {error, {empty_input, undefined}}
            end
    end;

validate({Options, Input}) ->
    case wpart_valid:is_private(Options) of
        true ->
	    {ok, Input};
        false ->
	    {value, {type, Type0}} = lists:keysearch(type, 1, Options),
	    Type = list_to_atom("wtype_" ++ atom_to_list(Type0)),

	    Delimiter = case lists:keysearch(delimiter, 1, Options) of
			    {_, {_, Del}} ->
				Del;
			    _ ->
				","
			end,

	   %%TODO: implement nesseccary options
	   %%{value, {max_val, Max_val}} = lists:keyse
	    Str = lists:keydelete(type, 1, Options), 
	    List = string:tokens(Input, Delimiter),
	    ResultList = lists:map(
			   fun(X) -> 				  
				   A = apply(Type, validate, [{Str,X}]),
				   B = tuple_to_list(A), 
				   if 
				       hd(B) =/= ok -> 
					   error;
				       true -> 
					   A
				   end
			   end, 
			   List),
	    case lists:member(error, ResultList) of
		true ->
		    {error, {wrong_value_in_set, Input}};
		false -> 
		    Ready = lists:map(fun({ok,X}) -> X end, ResultList),
		    %%Joined = string:join(Ready,",")
		    {ok,Ready}
	    end
    end.
	    
