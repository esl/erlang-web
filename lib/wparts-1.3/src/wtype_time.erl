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
%%% @author Michal Zajda <michal.ptaszek@erlang-consulting.com>
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(wtype_time).
-behaviour(wtype).

-include_lib("xmerl/include/xmerl.hrl").

-export([handle_call/2, validate/1]).
-export([get_time/2, is_valid_time/1]).
-export([check_min/2, check_max/2]).

handle_call(Format, #xmlText{value = Time}) -> 
    #xmlText{value = handle_call(Format, Time)};
handle_call(Format, Time) ->
    format(Format, Time, []).

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

validate({Options,Input}) ->
    case wpart_valid:is_private(Options) of
	true ->
	    {ok, Input};
	_ ->
	    Format = proplists:get_value(format, Options, "HH:MM:SS"),
	    case convert_input(Format, Input, []) of
		{error, bad_format} ->
		    {error, {bad_time_format, Input}};
		Time ->
		    case is_valid_time(Time) of
			true ->
			    case check_min(Options, Time) of
				{ok, Time} ->
				    check_max(Options, Time);
				ErrorMin ->
				    ErrorMin
			    end;
			false ->
			    {error, {not_valid_time, Time}}
		    end
	    end
    end.

check_min(Options, Time) ->
    case proplists:get_value(min, Options) of
	undefined ->
	    {ok, Time};
	Min when Time > Min ->
	    {ok, Time};
	_ ->
	    {error, {bad_range, Time}}
    end.

check_max(Options, Time) ->
    case proplists:get_value(max, Options) of
	undefined ->
	    {ok, Time};
	Max when Time < Max ->
	    {ok, Time};
	_ ->
	    {error, {bad_range, Time}}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
-spec(convert_input/3 :: (string(), string(), list(tuple())) -> 
	     {error, bad_format} | {tuple(), tuple()}).	
convert_input("HH" ++ Format, [I1, I2 | Input], Acc) ->
    case catch list_to_integer([I1, I2]) of
	{'EXIT', _} ->
	    {error, bad_format};
	Hour ->
	    convert_input(Format, Input, [{hour, Hour} | Acc])
    end;
convert_input("NN" ++ Format, [I1, I2 | Input], Acc) ->
    case catch list_to_integer([I1, I2]) of
	{'EXIT', _} ->
	    {error, bad_format};
	Minute ->
	    convert_input(Format, Input, [{minute, Minute} | Acc])
    end;
convert_input("SS" ++ Format, [I1, I2 | Input], Acc) ->
    case catch list_to_integer([I1, I2]) of
	{'EXIT', _} ->
	    {error, bad_format};
	Second ->
	    convert_input(Format, Input, [{second, Second} | Acc])
    end;
convert_input([_ | Format], [_ | Input], Acc) ->
    convert_input(Format, Input, Acc);
convert_input([], [], Acc) ->
    {proplists:get_value(hour, Acc, 0),
     proplists:get_value(minute, Acc, 0),
     proplists:get_value(second, Acc, 0)};
convert_input(_, _, _) ->
    {error, bad_format}.

is_valid_time({H1,H2,H3}) ->
    Hour = if (H1 >= 0) and (H1 < 24) -> true;
              true -> false
           end,

    Minute = if (H2 >= 0) and (H2 < 60) -> true;
              true -> false
           end,

    Sec = if (H3 >= 0) and (H3 < 60) -> true;
              true -> false
           end,

   lists:all(fun(X) -> X end, [Hour,Minute,Sec]);
is_valid_time(_) -> false.

get_time(Format, Time) ->
    format(Format, Time, []).

format("HH" ++ R, {Hour, _, _} = T, Acc) ->
    format(R, T, Acc ++ convert(Hour));
format("MM" ++ R, {_, Min, _} = T, Acc) ->
    format(R, T, Acc ++ convert(Min));
format("SS" ++ R, {_, _, Sec} = T, Acc) ->
    format(R, T, Acc ++ convert(Sec));
format([L | R], T, Acc) ->
    format(R, T, Acc ++ [L]);
format([], _, Acc) ->
    Acc.

convert(N) when N < 10 ->
    "0" ++ integer_to_list(N);
convert(N) ->
    integer_to_list(N).
