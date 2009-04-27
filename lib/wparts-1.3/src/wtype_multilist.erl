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
-module(wtype_multilist).
-behaviour(wtype).

-include_lib("xmerl/include/xmerl.hrl").
-export([handle_call/2,validate/1]).

handle_call(_Format, Val) -> 
    Val.

validate({Types, undefined}) ->
    case wpart_valid:is_private(Types) of
	true ->
	    {ok, undefined};
        false ->
            case lists:keysearch(optional, 1, Types) of
		{value, {optional, Default}} -> 
                    {ok, Default};
		_ ->  
                    {error, {empty_input, undefined}}
            end
    end;

validate({Types, Input}) ->
    case wpart_valid:is_private(Types) of
	true ->
	    one_element_check(Input);
	false ->
	    if 
		Input =/= undefined ->  
		    one_element_check(Input);
               true -> 
		    {error, {empty, Input}}
            end
    end;

validate(Input) -> {error, {it_is_not_multiple_list_value, Input}}.

one_element_check([T | _] = Input) when is_list(T) ->
    {ok, Input};
one_element_check([T | _] = Input) when is_integer(T) ->
    {ok, [Input]};
one_element_check([]) ->
    {error, {empty, []}}.
