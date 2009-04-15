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
%%% @author Martin Carlson <info@erlang-consulting.com>
%%%         Michal Zajda
%%% @doc 
%%% Generic datetime formatting of erlang datetime tuples
%%% @end
%%%-------------------------------------------------------------------
-module(wtype_date).
-behaviour(wtype).

-author("support@erlang-consulting.com").
-copyright("Erlang Training & Consulting Ltd.").

-include_lib("xmerl/include/xmerl.hrl").

%% API
-export([handle_call/2,validate/1,get_date/2]).

-export([check_min/2, check_max/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec handle_call(Format::string(), Datetime::tuple()) -> string()
%% @doc Returns a datetime string formatted according to `Format'
%% The format directive has place holders for the different parts
%% of the datetime tuple all other characters are passed on as is.
%% Year: YYYY YY
%% Month: MM SMONTH MONTH
%% Day: DD SDay DAY
%% Hour: HH
%% Minute: NN
%% Second: SS
%% @end 
handle_call(Format, #xmlText{value={_Date, _Time}} = XML) ->
    error_logger:warning_msg("~p module - this function is deprecated - "
			     "use wtype_datetime:handle_call instead~n", [?MODULE]),
    wtype_datetime:handle_call(Format, XML);
handle_call(Format, #xmlText{value=Val}) ->
    #xmlText{value=handle_call(Format, Val)};
handle_call([$X|_], {1970,1,1}) ->
    "N/A";
handle_call([$X|Format], Date) ->
    format(Format, Date, []);
handle_call(Format, Date) ->
    format(Format, Date, []).

format("YYYY" ++ T, {Year, _, _} = D, Acc) ->
    format(T, D, Acc ++ convert(Year, 4));
format("YY" ++ T, {Year, _, _} = D, Acc) ->
    format(T, D, Acc ++ convert(Year rem 100, 2));
format("MM" ++ T, {_, Month, _} = D, Acc) ->
    format(T, D, Acc ++ convert(Month, 2));
format("SMONTH" ++ T, {_, Month, _} = D, Acc) ->
    format(T, D, Acc ++ smonth(Month));
format("MONTH" ++ T, {_, Month, _} = D, Acc) ->
    format(T, D, Acc ++ month(Month));
format("DD" ++ T, {_, _, Day} = D, Acc) ->
    format(T, D, Acc ++ convert(Day, 2));
format("SDAY" ++ T, Date, Acc) ->
    format(T, Date, Acc ++ sday(Date));
format("DAY" ++ T, Date, Acc) ->
    format(T, Date, Acc ++ day(Date));
format([H|T], D, Acc) ->
    format(T, D, Acc ++ [H]);
format([], D, []) ->
    integer_to_list(calendar:datetime_to_gregorian_seconds({D, {0, 0, 0}}));
format([], _, Acc) ->
    Acc.

get_date(Format, Date) when is_tuple(Date) -> 
    format(Format, Date, []);
get_date(_, Date) when is_list(Date) ->
    Date.

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
	    Format = proplists:get_value(format, Options, "YYYY-MM-DD"),
	    case convert_input(Format, Input, []) of
		{error, bad_format} ->
		    {error, {bad_date_format, Input}};
		Date ->
		    case calendar:valid_date(Date) of
			true ->
			    case check_min(Options, Date) of
				{ok, Date} ->
				    check_max(Options, Date);
				ErrorMin ->
				    ErrorMin
			    end;
			false ->
			    {error, {not_valid_date, Input}}
		    end
	    end
    end.

check_min(Options, Date) ->
    case proplists:get_value(min, Options) of
	undefined ->
	    {ok, Date};
	Min when Date > Min ->
	    {ok, Date};
	_ ->
	    {error, {bad_range, Date}}
    end.

check_max(Options, Date) ->
    case proplists:get_value(max, Options) of
	undefined ->
	    {ok, Date};
	Max when Date < Max ->
	    {ok, Date};
	_ ->
	    {error, {bad_range, Date}}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
-spec(convert_input/3 :: (string(), string(), list(tuple())) -> 
	     {error, bad_format} | tuple()).	     
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
    {proplists:get_value(year, Acc, 0),
     proplists:get_value(month, Acc, 1),
     proplists:get_value(day, Acc, 1)};
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
    case lists:prefix(day0(N), Input) of
	true ->
	    {ok, lists:sublist(Input, length(day0(N))+1, length(Input))};
	false ->
	    skip_day(Input, N+1)
    end.

-spec(skip_sday/2 :: (string(), integer()) -> 
	     {error, bad_format} | {ok, string()}).
skip_sday(_, 8) ->
    {error, bad_format};
skip_sday(Input, N) ->
    case lists:prefix(sday0(N), Input) of
	true ->
	    {ok, lists:sublist(Input, length(sday0(N))+1, length(Input))};
	false ->
	    skip_sday(Input, N+1)
    end.   

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
smonth(6) -> "Jun";
smonth(7) -> "Jul";
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

-spec(sday/1 :: ({integer(), integer(), integer()}) -> string()).	     
sday(Date) ->
    sday0(calendar:day_of_the_week(Date)).

-spec(sday0/1 :: (integer()) -> string()).	     
sday0(1) -> "Mon";
sday0(2) -> "Tue";
sday0(3) -> "Wed";
sday0(4) -> "Thu";
sday0(5) -> "Fri";
sday0(6) -> "Sat";
sday0(7) -> "Sun".

-spec(day/1 :: ({integer(), integer(), integer()}) -> string()).	     
day(Date) ->
    day0(calendar:day_of_the_week(Date)).

day0(1) -> "Monday";
day0(2) -> "Tuesday";
day0(3) -> "Wednesday";
day0(4) -> "Thursday";
day0(5) -> "Friday";
day0(6) -> "Saturday";
day0(7) -> "Sunday".
