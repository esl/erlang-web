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
%% Ltd. Portions created by Erlang Training & Consulting Ltd are Copyright 2009,
%% Erlang Training & Consulting Ltd. All Rights Reserved.

%%%-------------------------------------------------------------------
%%% @version 1.0
%%% @author Michal Ptaszek <info@erlang-consulting.com>
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------

-module(wtype_collection).
-behaviour(wtype).

-include_lib("xmerl/include/xmerl.hrl").

-export([handle_call/2,validate/1]).

handle_call(_Format, #xmlText{value=String}) ->
    #xmlText{value=String};
handle_call(_Format, String) when is_list(String) ->
    String.

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

validate({Types, Values}) ->
    io:format("Types: ~p~n~nVals: ~p~n~n", [Types, Values]),
    {ok, Values}.
