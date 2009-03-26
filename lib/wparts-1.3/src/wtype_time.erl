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
%%% @author Michal Zajda <info@erlang-consulting.com>
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(wtype_time).
-behaviour(wtype).

-include_lib("xmerl/include/xmerl.hrl").

-export([handle_call/2, validate/1]).
-export([get_time/2, is_valid_time/1]).

handle_call(_Format, XML) -> XML.

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
	    Separators= [":"], 
	    
	    F = lists:keysearch(format, 1, Options),
	    
	    {value, {format,  Format}} = if 
					     F == false -> 
						 {value, {format, "HH:MM:SS"}};
					     true -> 
						 F
					 end,
	    
	    Length = length(Separator = lists:filter(
					  fun(X) -> string:str(Format, X) /= 0 
					  end, 
					  Separators)
			   ),
	    case Length of
		1   ->  Result = splitter(Input, Separator),
			{R, ResList} = Result,
			if ((R == ok) andalso (length(ResList) == 3) orelse (length(ResList) == 2)) ->
				{B,Inp} = check_limits(Options, Result, Separator, Format),
				if 
				    B -> 
					{ok,Inp};
				   true -> 
					{error, {bad_range, Input}}
				end;
			   true -> 
				{error, {bad_time_format, Input}}
			end;
		_   -> 
		    {error, {bad_separator_in_time_form, Input}}
	    end
    end.

%%====================================================================
%% Internal functions
%%====================================================================

splitter(Input, [Separator]) ->
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
                            splitter(Up_limit, Separator)
              end,

    Down_to_i = if D == false -> [];
                   true ->  {value, {min,  Down_limit}} = D, 
                             splitter(Down_limit, Separator)
                end,
 

    [Inside] = Separator,
    FormatStr = lists:concat(string:tokens(Format,Inside)),

    S = fun(X,Y) -> X =< Y end, 
    G = fun(X,Y) -> X >= Y end,

    calendar(FormatStr, Input_to_i, Up_to_i, Down_to_i,S,G).

%--------------------- handlers
calendar(_,_,{error,_},_,_,_) -> 
    {false, bad_up_limit_format};
calendar(_,_,_,{error,_},_,_) -> 
    {false, bad_down_limit_format};
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

%-------------------- reformaters
calendar("HHMMSS", Input, [], Fun) ->
    calendar(Input, [], Fun);

calendar("HHMMSS", Input, Limit, Fun) when length(Limit) == length(Input) ->
    calendar(Input, Limit, Fun);

calendar("HHMMSS", [H,M], Limit, Fun) -> 
    calendar([H,M,0], Limit, Fun);

calendar("HHMM", Input, Limit, Fun) when length(Limit) == length(Input) ->
    [H,M] = Input,
    [HL,ML] = Limit,
    calendar([H,M,0],[HL,ML,0], Fun);

calendar(_,I,_,_) -> 
    {false,I}.

%---------------------- engine
calendar(Input, [], _Fun)  when length(Input) == 3 ->
    Inp = list_to_tuple(Input),
    BI = is_valid_time(Inp),
    {BI,Inp};

calendar(Input, Limit, Fun) when length(Input) == 3 ->
    {H,H2,H3} = list_to_tuple(Input),
    {G,G2,G3} = list_to_tuple(Limit),
     
    BI = is_valid_time({H,H2,H3}),
    BL = is_valid_time({G,G2,G3}),
    
    Result = if 
                 BI and BL -> Fun({H,H2,H3},{G,G2,G3}) ;
                 true -> false
	     end,

    {Result,{H,H2,H3}};

calendar(_, _, _) ->
    {false, not_used}.

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

   lists:all(fun(X) -> X == true end, [Hour,Minute,Sec]);

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
