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
    format(Format, {Date,Time}, []);
handle_call(Format, {Date,Time}) ->
    format(Format, {Date,Time}, []).

format("Stamp", {D, T}) ->
    string:join(
      [string:join(lists:map(fun(X) -> integer_to_list(X) end,
 	                     tuple_to_list(D)), "-"),
       string:join(lists:map(fun(X) -> integer_to_list(X) end,
 	                     tuple_to_list(T)), ":")
      ], " ");

format(_, X) when is_list(X) ->
    X;

format(Format,{Date, Time}) ->
    format(Format, {Date, Time}, []);

format(_, _) ->
    "".

format({Date, Time}) ->
    format("YYYY-MM-DD HH:MM:SS", {Date, Time}, []).

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
	   case convert_input(Format, Input0, []) of
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
	    
format("YYYY" ++ T, {{Year, _, _}, _} = D, Acc) ->
    format(T, D, Acc ++ convert(Year, 4));
format("YY" ++ T, {{Year, _, _}, _} = D, Acc) ->
    format(T, D, Acc ++ convert(Year rem 100, 2));
format("MM" ++ T, {{_, Month, _}, _} = D, Acc) ->
    format(T, D, Acc ++ convert(Month, 2));
format("SMONTH" ++ T, {{_, Month, _}, _} = D, Acc) ->
    format(T, D, Acc ++ smonth(Month));
format("MONTH" ++ T, {{_, Month, _}, _} = D, Acc) ->
    format(T, D, Acc ++ month(Month));
format("DD" ++ T, {{_, _, Day}, _} = D, Acc) ->
    format(T, D, Acc ++ convert(Day, 2));
format("SDAY" ++ T, {Date, _} = D, Acc) ->
    format(T, D, Acc ++ sday(Date));
format("DAY" ++ T, {Date, _} = D, Acc) ->
    format(T, D, Acc ++ day(Date));
format("HH" ++ T, {_, {Hour, _, _}} = D, Acc) ->
    format(T, D, Acc ++ convert(Hour, 2));
format("NN" ++ T, {_, {_, Min, _}} = D, Acc) ->
    format(T, D, Acc ++ convert(Min, 2));
format("SS" ++ T, {_, {_, _, Sec}} = D, Acc) ->
    format(T, D, Acc ++ convert(Sec, 2));
format([H|T], D, Acc) ->
    format(T, D, Acc ++ [H]);
format([], D, []) ->
    Ref = {{1970, 1, 1}, {0, 0, 0}},
    integer_to_list(calendar:datetime_to_gregorian_seconds(D) -
                    calendar:datetime_to_gregorian_seconds(Ref));
format([], _, Acc) ->
    Acc.

convert(N, Len) ->
    case integer_to_list(N) of
        List when length(List) >= Len ->
            List;
        List ->
            [$0||_ <- lists:seq(1, Len - length(List))] ++ List
    end.

smonth(1) -> "Jan";
smonth(2) -> "Feb";
smonth(3) -> "Mar";
smonth(4) -> "Apr";
smonth(5) -> "May";
smonth(6) -> "June";
smonth(7) -> "July";
smonth(8) -> "Aug";
smonth(9) -> "Sep";
smonth(10) -> "Oct";
smonth(11) -> "Nov";
smonth(12) -> "Dec".

month(1) -> "January";
month(2) -> "Febuary";
month(3) -> "March";
month(4) -> "April";
month(5) -> "May";
month(6) -> "June";
month(7) -> "July";
month(8) -> "August";
month(9) -> "September";
month(10) -> "October";
month(11) -> "Novmber";
month(12) -> "December".

sday(1) -> "Mon";
sday(2) -> "Tue";
sday(3) -> "Wed";
sday(4) -> "Thu";
sday(5) -> "Fri";
sday(6) -> "Sat";
sday(7) -> "Sun";
sday(D) -> sday(calendar:day_of_the_week(D)).

day(1) -> "Monday";
day(2) -> "Tuesday";
day(3) -> "Wednesday";
day(4) -> "Thursday";
day(5) -> "Friday";
day(6) -> "Saturday";
day(7) -> "Sunday";
day(D) -> day(calendar:day_of_the_week(D)).

-spec(convert_input/3 :: (string(), string(), list(tuple())) -> 
	     {error, bad_format} | {tuple(), tuple()}).	     
convert_input("YYYY" ++ Format, [I1, I2, I3, I4 | Input], Acc) ->
    case catch list_to_integer([I1, I2, I3, I4]) of
	{'EXIT', _} ->
	    {error, bad_format};
	Year ->
	    convert_input(Format, Input, [{year, Year} | Acc])
    end;
convert_input("YY" ++ Format, [I1, I2 | Input], Acc) ->
    case catch list_to_integer([I1, I2]) of
	{'EXIT', _} ->
	    {error, bad_format};
	Year when Year >= 70 -> %% epoch year
	    convert_input(Format, Input, [{year, 2000 + Year} | Acc]);
	Year ->
	    convert_input(Format, Input, [{year, 1900 + Year} | Acc])
    end;
convert_input("MM" ++ Format, [I1, I2 | Input], Acc) ->
    case catch list_to_integer([I1, I2]) of
	{'EXIT', _} ->
	    {error, bad_format};
	Month ->
	    convert_input(Format, Input, [{month, Month} | Acc])
    end;
convert_input("DD" ++ Format, [I1, I2 | Input], Acc) ->
    case catch list_to_integer([I1, I2]) of
	{'EXIT', _} ->
	    {error, bad_format};
	Day ->
	    convert_input(Format, Input, [{day, Day} | Acc])
    end;
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
convert_input("SMONTH" ++ Format, Input, Acc) ->
    case smonth_to_int(Input, 1) of
	{error, bad_format} ->
	    {error, bad_format};
	{Smonth, NewInput} ->
	    convert_input(Format, NewInput, [{month, Smonth} | Acc])
    end;
convert_input("MONTH" ++ Format, Input, Acc) ->
    case month_to_int(Input, 1) of
	{error, bad_format} ->
	    {error, bad_format};
	{Month, NewInput} ->
	    convert_input(Format, NewInput, [{month, Month} | Acc])
    end;
convert_input("DAY" ++ Format, Input, Acc) ->
    case skip_day(Input, 1) of
	{ok, NewInput} ->
	    convert_input(Format, NewInput, Acc);
	Else ->
	    Else
    end;
convert_input("SDAY" ++ Format, Input, Acc) ->
    case skip_sday(Input, 1) of
	{ok, NewInput} ->
	    convert_input(Format, NewInput, Acc);
	Else ->
	    Else
    end;
convert_input([_ | Format], [_ | Input], Acc) ->
    convert_input(Format, Input, Acc);
convert_input([], [], Acc) ->
    {{proplists:get_value(year, Acc, 0),
      proplists:get_value(month, Acc, 1),
      proplists:get_value(day, Acc, 1)},
     {proplists:get_value(hour, Acc, 0),
      proplists:get_value(minute, Acc, 0),
      proplists:get_value(second, Acc, 0)}};
convert_input(_, _, _) ->
    {error, bad_format}.

-spec(smonth_to_int/2 :: (string(), integer()) -> 
	     {error, bad_format} | {integer(), string()}).
smonth_to_int(_, 13) ->
    {error, bad_format};
smonth_to_int(Input, N) ->
    case lists:prefix(smonth(N), Input) of
	true ->
	    {N, lists:sublist(Input, length(smonth(N))+1, length(Input))};
	false ->
	    smonth_to_int(Input, N+1)
    end.

-spec(month_to_int/2 :: (string(), integer()) -> 
	     {error, bad_format} | {integer(), string()}).
month_to_int(_, 13) ->
    {error, bad_format};
month_to_int(Input, N) ->
    case lists:prefix(month(N), Input) of
	true ->
	    {N, lists:sublist(Input, length(month(N))+1, length(Input))};
	false ->
	    month_to_int(Input, N+1)
    end.

-spec(skip_day/2 :: (string(), integer()) -> 
	     {error, bad_format} | {ok, string()}).
skip_day(_, 8) ->
    {error, bad_format};
skip_day(Input, N) ->
    case lists:prefix(day(N), Input) of
	true ->
	    {ok, lists:sublist(Input, length(day(N))+1, length(Input))};
	false ->
	    skip_day(Input, N+1)
    end.

-spec(skip_sday/2 :: (string(), integer()) -> 
	     {error, bad_format} | {ok, string()}).
skip_sday(_, 8) ->
    {error, bad_format};
skip_sday(Input, N) ->
    case lists:prefix(sday(N), Input) of
	true ->
	    {ok, lists:sublist(Input, length(sday(N))+1, length(Input))};
	false ->
	    skip_sday(Input, N+1)
    end.
