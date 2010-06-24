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
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(wtype_bool).
-behaviour(wtype).

-export([handle_call/2, validate/1]).

-include_lib("xmerl/include/xmerl.hrl").

handle_call(_Format, true) ->
    "true";
handle_call(_Format, false) ->
    "false";
handle_call(_Format, #xmlText{value=Val}) -> 
    #xmlText{value=handle_call(not_used, Val)}.

validate({Types, Val}) ->
    case wpart_valid:is_private(Types) of
	true ->
	    {ok, Val};
	false ->
	    Val1 = format_val(Val),
	    check_always(proplists:get_value(always, Types, []), Val1)
    end.

-spec(check_always/2 :: (list(string()), list(string())) -> {ok, list(string())} | {error, {not_all_mandatory_fields_checked, list(string())}}).	     
check_always([], Vals) ->
    {ok, Vals};
check_always(Always, Vals) ->
    case lists:all(fun(Val) ->
			   lists:member(Val, Vals)
		   end, Always) of
	true ->
	    {ok, Vals};
	false ->
	    {error, {not_all_mandatory_fields_checked, Vals}}
    end.

-spec(format_val/1 :: (undefined | string() | list(string())) -> list(string())).	     
format_val(undefined) ->
    [];
format_val(Val) when is_integer(hd(Val)) ->
    [Val];
format_val(Val) ->
    Val.
