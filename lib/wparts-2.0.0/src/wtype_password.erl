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
-module(wtype_password).
-behaviour(wtype).

-export([handle_call/2,validate/1]).

-include_lib("xmerl/include/xmerl.hrl").

handle_call(Format, #xmlText{value=String}) ->
    #xmlText{value=handle_call(Format, String)};
handle_call("stars", String) ->
    [$* || _ <- String];
handle_call(_, String) ->
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

validate({Types, [String1, String2]}) 
  when is_list(String1) and is_list(String2) ->
    validate2({Types, [String1, String2]});

validate({Types, String}) when is_list(String) ->
    validate2({Types, String}).

validate2({Types, String}) when is_list(String) ->
    case wpart_valid:is_private(Types) of
	true ->
	    {ok, String};
	false ->
	    case check_confirm(String, Types) of
		{ok, NString} ->
		    case check_min_length(NString, Types) of
			{ok, NString} ->
			    case check_max_length(NString, Types) of
				{ok, NString} -> 
				    check_regexp(NString, Types);
				ErrorMax -> 
				    ErrorMax
			    end;
			ErrorMin -> ErrorMin
		    end;
		ErrorConfirm ->
		    ErrorConfirm
	    end
    end.

check_confirm(String, Types) ->
    check_confirm0(String, proplists:get_value(confirm, Types, false)).

check_confirm0(String, false) ->
    {ok, String};
check_confirm0([String, String], true) when is_list(String) ->
    {ok, String};
check_confirm0(String, _) ->
    {error, {passwords_do_not_match, String}}.
    
check_min_length(String, Types) ->
    case lists:keysearch(min_length, 1, Types) of
	{value, {min_length, Min}} ->
            N = length(String),
    	    if
		N < Min ->
		    {error, {too_short, String}};
		true -> 		    
		    {ok, String}
	    end;
	_ -> 
	    {ok, String}
    end.

check_max_length(String, Types) ->
    case lists:keysearch(max_length, 1, Types) of
	{value, {max_length, Max}} ->
            N = length(String),
	    if
		N > Max ->
		    {error, {too_long, String}};
		true -> 
		    {ok, String}
	    end;
	_ -> 
	    {ok, String}
    end.

check_regexp(String, Types) ->
    case lists:keysearch(regexp, 1, Types) of
	{value, {regexp, Regexp}} ->
	    case re:run(String, Regexp, [unicode]) of
		{match, _} ->
		    {ok, String};
		nomatch ->
		    {error, {regexp_does_not_match, String}}
	    end;
	_ ->
	    {ok, String}
    end.
