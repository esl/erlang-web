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
    error_logger:warning_msg("~p module - this function is deprecated - use wtype_datetime:handle_call instead~n", [?MODULE]),
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
	    Separators= ["-","/"," ",".", "_"], 
	    F = lists:keysearch(format, 1, Options),
	    {value, {format,  Format}} = if F == false -> {value, {format, "YYYY-MM-DD"}};
					    true -> F
					 end,
	    Length = length(Separator = lists:filter(
					  fun(X) -> string:str(Format, X) /= 0 
					  end, 
					  Separators)
			   ),
	    case Length of
		1   ->  Result = splitter(Input, Format, Separator),
			{R, ResList} = Result,
			if ((R == ok) andalso (length(ResList))) == 3 ->
				{B,Inp} = check_limits(Options, Result),
				if B -> {ok,Inp};
				   true -> {error, {bad_range, Input}}
				end;
			   true -> {error, {bad_date_format, Input}}
			end;
		_   -> {error, {bad_separator_in_date_form, Input}}
	    end
    end.

%%====================================================================
%% Internal functions
%%====================================================================

splitter(Input, Format, [Separator]) ->
    {ok, DateList} = regexp:split(Input, Separator),
    {ok, FormatList} = regexp:split(Format, Separator),
    Input_to_i = validate_field(DateList, FormatList, []),
    case lists:all(fun is_integer/1, Input_to_i) of
        true -> {ok, Input_to_i};
        _ -> {error, {bad_date_input, Input}}	
    end.

validate_field([],[],List) ->
    lists:map(fun(L) ->
		      element(2, L)
	      end, lists:keysort(1, List));
validate_field([], [_|_], _) ->
    [error];
validate_field([_|_], [], _) ->
    [error];
validate_field([H1|T1],[H2|T2],List) ->
    R = field(H2, H1),
    validate_field(T1,T2,[R|List]).

field("YYYY", Value) ->
    catch {1, list_to_integer(Value)};
field("YY", Value) ->
    catch {1, list_to_integer(Value)};
field("MM", Value) ->
    catch {2, list_to_integer(Value)};
field("DD", Value) ->
    catch {3, list_to_integer(Value)};
field("SMONTH", Value) ->
    {2, smonth_to_int(Value)};
field("MONTH", Value) ->
    {2, month_to_int(Value)};
field("SDAY", Value) ->
    {3, sday_to_int(Value)};
field("DAY", Value) ->
    {3, day_to_int(Value)};
field(_Format, _Value) ->
    bad_format.

check_limits(_, {error, X}) -> {error, X};

check_limits(Options, {ok, Input_to_i}) -> 
    U = lists:keysearch(max, 1, Options),
    D = lists:keysearch(min, 1, Options),

    Limit = 
	fun(Val) ->
		case Val of
		    false -> [];
		    {value, {max, {H,M,S}}} -> 
			case lists:all(fun is_integer/1, [H,M,S]) of
			    true -> {ok, {H,M,S}};
			    _ -> {error, {bad_limit_format, {H,M,S}}}	
			end;
		    _ -> {error, {bad_limit_format, Val}}
		end
	end,
    MaxFun = fun(X,Y) -> X =< Y end, 
    MinFun = fun(X,Y) -> X >= Y end,
    test_limits(list_to_tuple(Input_to_i), Limit(D), Limit(U), MinFun, MaxFun).
%----------------------------------------------------------------------------
test_limits(_,{error,_},_,_,_) -> {false, bad_min_limit_format};
test_limits(_,_,{error,_},_,_) -> {false, bad_max_limit_format};
test_limits(Input, [], {ok,Max}, _, MaxFun) ->
    Result = test_limit(Input, Max, MaxFun),
    {Result, Input};
test_limits(Input, {ok,Min}, [], MinFun, _) ->
    Result = test_limit(Input, Min, MinFun),
    {Result, Input};
test_limits(Input, [], [],_,_) ->
    {true, Input};
test_limits(Input, {ok,Min}, {ok,Max}, MinFun, MaxFun) ->
    R1 = test_limit(Input, Min, MinFun),
    R2 = test_limit(Input, Max, MaxFun),
    Result = if R1 and R2 -> true;
		true -> false
	     end,
    {Result,Input}.
    
test_limit(Input, Limit, LimitFun) ->
    BI = calendar:valid_date(Input),
    BL = calendar:valid_date(Limit),
    if BI and BL ->
	    GI = calendar:date_to_gregorian_days(Input),
	    GL = calendar:date_to_gregorian_days(Limit),
	    LimitFun(GI,GL);
       true -> false
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

smonth_to_int("Jan") -> 1;
smonth_to_int("Feb") -> 2;
smonth_to_int("Mar") -> 3;
smonth_to_int("Apr") -> 4;
smonth_to_int("May") -> 5;
smonth_to_int("Jun") -> 6;
smonth_to_int("Jul") -> 7;
smonth_to_int("Aug") -> 8;
smonth_to_int("Sep") -> 9;
smonth_to_int("Oct") -> 10;
smonth_to_int("Nov") -> 11;
smonth_to_int("Dec") -> 12;
smonth_to_int(_) -> bad_month.
    
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

month_to_int("January") -> 1;
month_to_int("February") -> 2;
month_to_int("March") -> 3;
month_to_int("April") -> 4;
month_to_int("May") -> 5;
month_to_int("June") -> 6;
month_to_int("July") -> 7;
month_to_int("August") -> 8;
month_to_int("September") -> 9;
month_to_int("October") -> 10;
month_to_int("November") -> 11;
month_to_int("December") -> 12;
month_to_int(_) -> bad_month.

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

sday_to_int("Mon") -> 1;
sday_to_int("Tue") -> 2;
sday_to_int("Wed") -> 3;
sday_to_int("Thu") -> 4;
sday_to_int("Fri") -> 5;
sday_to_int("Sat") -> 6;
sday_to_int("Sun") -> 7;
sday_to_int(_) -> bad_day.

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

day_to_int("Monday") -> 1;
day_to_int("Tuesday") -> 2;
day_to_int("Wednesday") -> 3;
day_to_int("Thursday") -> 4;
day_to_int("Friday") -> 5;
day_to_int("Saturday") -> 6;
day_to_int("Sunday") -> 7;
day_to_int(_) -> bad_day.
