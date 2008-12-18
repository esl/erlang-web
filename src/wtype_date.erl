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
-vsn("$Rev").

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

get_date(Format, Date) -> 
    format(Format, Date, []). 

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
	   1   ->  Result = spliter(Input, Separator),
                 {R, ResList} = Result,
                    if ((R == ok) andalso (length(ResList))) == 3 ->
                            {B,Inp} = check_limits(Options, Result, Separator, Format),
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

spliter(Input, [Separator]) ->
    {ok, DateList} = regexp:split(Input, Separator),	
    Input_to_i = lists:map(
                            fun(L) -> 
                               catch list_to_integer(L) 
                            end,
                            DateList),
    
    case lists:all(fun is_integer/1, Input_to_i) of
        true -> {ok, Input_to_i};
        _ -> {error, {bad_date_input, Input}}	
    end.

check_limits(_, {error, X}, _, _) -> {error, X};

check_limits(Options, {ok, Input_to_i}, Separator, Format) -> 

    U = lists:keysearch(max, 1, Options),
    D = lists:keysearch(min, 1, Options),

    Up_to_i = if U == false -> [];
                 true ->    {value, {max,  Up_limit}} = U,
                            spliter(Up_limit, Separator)
              end,

    Down_to_i = if D == false -> [];
                   true ->  {value, {min,  Down_limit}} = D, 
                             spliter(Down_limit, Separator)
                end,

    [Inside] = Separator,
    FormatStr = lists:concat(string:tokens(Format,Inside)),

    S = fun(X,Y) -> X =< Y end, 
    G = fun(X,Y) -> X >= Y end,

    calendar(FormatStr, Input_to_i, Up_to_i, Down_to_i,S,G).
%----------------------------------------------------------------------------
calendar(_,_,{error,_},_,_,_) -> {false, bad_up_limit_format};
calendar(_,_,_,{error,_},_,_) -> {false, bad_down_limit_format};

calendar(Format, Input, [], [],F,_) -> 
        calendar(Format, Input, [], F);

calendar(Format, Input_to_i, {ok,Up_to_i}, [], S, _G) ->
        calendar(Format, Input_to_i, Up_to_i, S);

calendar(Format, Input_to_i, [], {ok,Down_to_i},_S,G) ->
       calendar(Format, Input_to_i, Down_to_i, G);

calendar(Format, Input_to_i, {ok,Up_to_i}, {ok,Down_to_i},S,G) ->
       {R1,Inp1} = calendar(Format, Input_to_i, Up_to_i, S),
       {R2,_Inp2} = calendar(Format, Input_to_i, Down_to_i, G),
       {lists:all(fun(X) -> X == true end, [R1,R2]), Inp1}.

%-----------------------------------------------------------------------------

calendar("YYYY"++ T, Input, Limit, Fun) ->
        if T == "MMDD" -> calendar(Input, Limit, Fun);
           T == "DDMM" -> [H1|T1] = Input,
                          calendar([H1|lists:reverse(T1)],Limit,Fun);
           true -> {false, Input}
        end;

calendar("DD"++ T, Input, Limit, Fun) ->
        if T == "MMYYYY" -> calendar(lists:reverse(Input),Limit, Fun);
           true -> {false, Input}
        end;

calendar("MM"++ T, Input, Limit, Fun) ->
        if T == "DDYYYY" -> [H1|T1] = Input, 
                            [HH1|TT1] = T1,
                            [Year1|_] = TT1,
                            calendar([Year1,H1,HH1], Limit, Fun);
           true -> {false, Input}
        end.
%--------------------------------------------------------------------------

calendar(Input, [], _Fun) ->
    Inp = list_to_tuple(Input),
    BI = calendar:valid_date(Inp),
    {BI,Inp};

calendar(Input, Limit, Fun) ->
    Inp = list_to_tuple(Input),
    Lmt = list_to_tuple(Limit),
    
    BI = calendar:valid_date(Inp),
    BL = calendar:valid_date(Lmt),
    
    Result = 
    if BI and BL ->  NI = calendar:date_to_gregorian_days(Inp),
                     NL = calendar:date_to_gregorian_days(Lmt), 
                     Fun(NI,NL);
       true -> false
    end, 
    
    %change: List -> Tuple
    {Result,Inp}.



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
