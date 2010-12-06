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
%% The Initial Developer of the Original Code is Erlang Solutions Ltd.
%% Portions created by Erlang Solutions Ltd. are Copyright 2010,
%% Erlang Solutions Ltd. All Rights Reserved.

%%%-------------------------------------------------------------------
%%% @author Krzysztof Goj <krzysztof.goj@erlang-solutions.com>
%%% @doc Utility functions for date tuple to string (and viceversa) conversion
%%% This code is meant to be wrapped by wtypes, not to be used on it's own.
%%% @end
%%%-------------------------------------------------------------------
-module(wpart_time_str).

% API
-export([format/2, parse_input/2]).

%%====================================================================
%% API
%%====================================================================
-spec(format/2 :: (string(), {(tuple() | unused), (tuple() | unused)}) -> string()).
format(_, X) when is_list(X) ->
    X;
format(Format, {Date, Time}) ->
    format(Format, {Date, Time}, []);
format(_, _) ->
    "".

-spec(parse_input/2 :: (string(), string()) ->
	     {error, bad_format} | [{atom(), integer()}]).
parse_input(Format, Input) ->
    parse_input(Format, Input, []).

%%====================================================================
%% Internal functions
%%====================================================================

format([$\\,Escaped|T], D, Acc) ->
    format(T, D, Acc ++ [Escaped]);
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
month(2) -> "February";
month(3) -> "March";
month(4) -> "April";
month(5) -> "May";
month(6) -> "June";
month(7) -> "July";
month(8) -> "August";
month(9) -> "September";
month(10) -> "October";
month(11) -> "November";
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

-spec(parse_input/3 :: (string(), string(), list(tuple())) ->
	     {error, bad_format} | [{atom(), integer()}]).
parse_input("YYYY" ++ Format, [I1, I2, I3, I4 | Input], Acc) ->
    case catch list_to_integer([I1, I2, I3, I4]) of
	{'EXIT', _} ->
	    {error, bad_format};
	Year ->
	    parse_input(Format, Input, [{year, Year} | Acc])
    end;
parse_input("YY" ++ Format, [I1, I2 | Input], Acc) ->
    case catch list_to_integer([I1, I2]) of
	{'EXIT', _} ->
	    {error, bad_format};
	Year when Year >= 70 -> %% epoch year
	    parse_input(Format, Input, [{year, 2000 + Year} | Acc]);
	Year ->
	    parse_input(Format, Input, [{year, 1900 + Year} | Acc])
    end;
parse_input("MM" ++ Format, [I1, I2 | Input], Acc) ->
    case catch list_to_integer([I1, I2]) of
	{'EXIT', _} ->
	    {error, bad_format};
	Month ->
	    parse_input(Format, Input, [{month, Month} | Acc])
    end;
parse_input("DD" ++ Format, [I1, I2 | Input], Acc) ->
    case catch list_to_integer([I1, I2]) of
	{'EXIT', _} ->
	    {error, bad_format};
	Day ->
	    parse_input(Format, Input, [{day, Day} | Acc])
    end;
parse_input("HH" ++ Format, [I1, I2 | Input], Acc) ->
    case catch list_to_integer([I1, I2]) of
	{'EXIT', _} ->
	    {error, bad_format};
	Hour ->
	    parse_input(Format, Input, [{hour, Hour} | Acc])
    end;
parse_input("NN" ++ Format, [I1, I2 | Input], Acc) ->
    case catch list_to_integer([I1, I2]) of
	{'EXIT', _} ->
	    {error, bad_format};
	Minute ->
	    parse_input(Format, Input, [{minute, Minute} | Acc])
    end;
parse_input("SS" ++ Format, [I1, I2 | Input], Acc) ->
    case catch list_to_integer([I1, I2]) of
	{'EXIT', _} ->
	    {error, bad_format};
	Second ->
	    parse_input(Format, Input, [{second, Second} | Acc])
    end;
parse_input("SMONTH" ++ Format, Input, Acc) ->
    case smonth_to_int(Input, 1) of
	{error, bad_format} ->
	    {error, bad_format};
	{Smonth, NewInput} ->
	    parse_input(Format, NewInput, [{month, Smonth} | Acc])
    end;
parse_input("MONTH" ++ Format, Input, Acc) ->
    case month_to_int(Input, 1) of
	{error, bad_format} ->
	    {error, bad_format};
	{Month, NewInput} ->
	    parse_input(Format, NewInput, [{month, Month} | Acc])
    end;
parse_input("DAY" ++ Format, Input, Acc) ->
    case skip_day(Input, 1) of
	{ok, NewInput} ->
	    parse_input(Format, NewInput, Acc);
	Else ->
	    Else
    end;
parse_input("SDAY" ++ Format, Input, Acc) ->
    case skip_sday(Input, 1) of
	{ok, NewInput} ->
	    parse_input(Format, NewInput, Acc);
	Else ->
	    Else
    end;
parse_input([_ | Format], [_ | Input], Acc) ->
    parse_input(Format, Input, Acc);
parse_input([], [], Acc) ->
    Acc;
parse_input(_, _, _) ->
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
