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

-module(wtype_float).
-behaviour(wtype).

-include_lib("xmerl/include/xmerl.hrl").

-export([handle_call/2,validate/1]).

handle_call(_Format, #xmlText{value=Float}) ->
    #xmlText{value=float_to_list(Float)};
handle_call(_Format, Float) when is_float(Float) ->
    float_to_list(Float).

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

validate({Types,Input}) ->
    case wpart_valid:is_private(Types) of
	true ->
	    {ok, Input};
	false ->
		case string:to_float(Input) of 
		    {Flt, []} -> 
			case check_min(Flt, Types) of
			    {ok, Flt} ->
				case check_max(Flt, Types) of
				    {ok, Flt} -> {ok, Flt};
				    ErrorMax -> ErrorMax
				end;
			    ErrorMin -> ErrorMin
			end;
		    {error, no_float} ->
			case string:to_integer(Input) of
			    {Int, []} ->
				Flt1 = Int * 1.0,
				case check_min(Flt1, Types) of
				    {ok, Flt1} ->
					case check_max(Flt1, Types) of
					    {ok, Flt1} -> {ok, Flt1};
					    ErrorMax -> ErrorMax
					end;
				    ErrorMin -> ErrorMin
				end;
			    _ ->
				{error, {not_float, Input}}
			end;
		    _ -> 
			{error, {not_float, Input}}
		end
    end.

check_min(Input, Types) ->
    case lists:keysearch(min, 1, Types) of
	{value, {min, Min}} ->
	    if
		Input < Min ->
		    {error, {smaller_than_min, Input}};
		true ->
		    {ok, Input}
	    end;
	_ ->
	    {ok, Input}
    end.

check_max(Input, Types) ->
    case lists:keysearch(max, 1, Types) of
	{value, {max, Max}} ->
	    if
		Input > Max ->
		    {error, {greater_than_max, Input}};
		true ->
		    {ok, Input}
	    end;
	_ ->
	    {ok, Input}
    end.
