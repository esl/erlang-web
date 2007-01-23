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
%%% @author Michal Zajda <michal.zajda@erlang-consulting.com>
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(wtype_datetime).
-behaviour(wtype).

-include_lib("xmerl/include/xmerl.hrl").

-export([handle_call/2, validate/1, format/2, format/1]).

handle_call(Format, #xmlText{value=Val}) ->
    #xmlText{value=handle_call(Format, Val)};
handle_call([$X|_], {{1970,1,1},{0,0,0}}) ->
    "N/A";
handle_call([$X|Format], {Date,Time}) ->
    format(Format, {Date,Time});
handle_call(Format, {Date,Time}) ->
    format(Format, {Date,Time}).

format("Stamp", {D, T}) ->
    string:join(
      [string:join(lists:map(fun(X) -> integer_to_list(X) end,
 	                     tuple_to_list(D)), "-"),
       string:join(lists:map(fun(X) -> integer_to_list(X) end,
 	                     tuple_to_list(T)), ":")
      ], " ");

format(Format, {Date, Time}) ->
    wpart_time_str:format(Format, {Date, Time}).

format({Date, Time}) ->
    format("YYYY-MM-DD HH:NN:SS", {Date, Time}).

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

validate({Options, Input0}) ->
   case wpart_valid:is_private(Options) of
        true ->
	       {ok, Input0};
        _ ->
            Format = proplists:get_value(format, Options, "YYYY-MM-DD HH:NN:SS"),

            Input1 = case convert_input(Format, Input0) of
                {error, bad_format} ->
                    AltFormat = proplists:get_value(alt_format, Options, "YYYY-MM-DD"),
                    convert_input(AltFormat, Input0);
                Inp -> Inp
            end,

            case Input1 of
                {error, bad_format} ->
                    {error, {bad_format, Input0}};
                {Date, _} = Input ->
                    case calendar:valid_date(Date) of
                        true ->
                            case check_min(Options, Input) of
                                {ok, Input} ->
                                    check_max(Options, Input);
                                ErrorMin ->
                                    ErrorMin
                            end;
                        false ->
                            {error, {not_valid_date, Input}}
                    end
            end
    end.

-spec(check_min/2 :: (list(), {tuple(), tuple()}) -> 
	     {ok, {tuple(), tuple()}} | {error, {term(), tuple()}}).
check_min(Options, In) ->
    case proplists:get_value(min, Options) of
	undefined ->
	    {ok, In};
	DT when DT < In ->
	    {ok, In};
	_ ->
	    {error, {bad_range, In}}
    end.

check_max(Options, In) ->
    case proplists:get_value(max, Options) of
	undefined ->
	    {ok, In};
	DT when DT > In ->
	    {ok, In};
	_ ->
	    {error, {bad_range, In}}
    end.

-spec(convert_input/2 :: (string(), string()) -> 
	     {error, bad_format} | {tuple(), tuple()}).	     
convert_input(Format, Input) ->
    case wpart_time_str:parse_input(Format, Input) of
        {error, bad_format} ->
            {error, bad_format};
        Values ->
            {{proplists:get_value(year, Values, 0),
              proplists:get_value(month, Values, 1),
              proplists:get_value(day, Values, 1)},
             {proplists:get_value(hour, Values, 0),
              proplists:get_value(minute, Values, 0),
              proplists:get_value(second, Values, 0)}}
    end.
