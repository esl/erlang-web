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
%%% @version $Rev$
%%% @author Michal Zajda <info@erlang-consulting.com>
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
    format(Format, {Date, Time}, []).

format({Date, Time}) ->
    format("YYYY-MM-DD HH:MM:SS", {Date, Time}, []).

%% {[{format, "YYYY-MM-DD HH:MM:SS"}, {min, "2009-2-23 12:23:33"}], "2009-7-23 23:12:23"}.
%% accepts no time part in the input
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

validate({Options, Input}) ->
   case wpart_valid:is_private(Options) of
        true ->
	       {ok, Input};
        _ ->
               F = lists:keysearch(format, 1, Options),
               {value, {format,  Format}} = if F == false -> 
                                                    {value, {format, "YYYY-MM-DD HH:MM:SS"}};
                                       true -> F
                              end,  

               Min = lists:keysearch(min, 1, Options),
              
               {value, {min,  MinVal}} = if Min == false -> {value, {min,  false}};
                                            true -> Min
                              end,
 
               Max = lists:keysearch(max, 1, Options),
               {value, {max,  MaxVal}} = if Max == false -> {value, {max,  false}};
                                            true -> Max
                              end, 

               {MinD, MinT} = if Min =/= false -> {ok, [Min1,Min2]} = regexp:split(MinVal, " "),
                                                  {{min, Min1},{min, Min2}};
                                  true -> {false,false}
                              end,
               {MaxD, MaxT} = if Max =/= false -> {ok, [Max1,Max2]} = regexp:split(MaxVal, " "),
                                                  {{max, Max1},{max, Max2}};
                                  true -> {false,false}
                              end,

               {ok, [F1,F2]} = regexp:split(Format, " "),

               {R1, X} = regexp:split(Input, " "),

               if (R1 == ok) and (length(X) == 2) ->
                    [X1,X2] = X,
                    {R2, Date} = wtype_date:validate({[{format,F1},MinD,MaxD],X1}),
                    if R2 =/= ok -> {error, {error, bad_date}};
                       true -> 
                            {R3, Time} =  wtype_time:validate({[{format,F2},
                                                                MinT,MaxT],X2}), 
                            if R3 == ok ->
                                    {ok,{Date, Time}};
                               true -> {error, {bad_time, Input}}
                            end
                    end;
                  (R1 == ok) and (length(X) == 1) ->
                     [Y] = X,
                     {R2, Date} = wtype_date:validate({[{format,F1},MinD,MaxD],Y}),
                      if R2 =/= ok -> {error, {bad_input, Input}};
                         true -> {ok,{Date, {0,0,0}}}
                      end;

	          true -> {error, {bad_input, Input}}

                end
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
