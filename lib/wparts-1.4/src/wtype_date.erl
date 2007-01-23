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
handle_call(Format, Date) ->
    get_date(Format, Date).

get_date(Format, Date) when is_tuple(Date) -> 
    wpart_time_str:format(Format, {Date, unused});
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
	    case convert_input(Format, Input) of
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

-spec(convert_input/2 :: (string(), string()) ->
	     {error, bad_format} | {integer(), integer(), integer()}).
convert_input(Format, Input) ->
    case wpart_time_str:parse_input(Format, Input) of
        {error, bad_format} ->
            {error, bad_format};
        Values ->
            {proplists:get_value(year, Values, 0),
             proplists:get_value(month, Values, 0),
             proplists:get_value(day, Values, 0)}
    end.
