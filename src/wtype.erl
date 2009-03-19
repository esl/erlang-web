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
%%% @author Martin Carlson <martin@erlang-consulting.com>
%%% @doc Behaviour module for wtypes.
%%% The <i>wtype</i> behaviour defines two callback functions:
%%% <ul>
%%% <li><i>handle_call(Format, Value)</i> - converts the passed <i>Value</i>
%%% according to the <i>Format</i>. It is used when the value kept in 
%%% request dictionary must be preformated before embedding in the view
%%% (like integer -> string convertions).</li>
%%% <li><i>validate({Types, Input})</i> - does the actual validation. Function
%%% takes the <i>Types</i> - list of the parameters describing and limiting 
%%% the <i>Input</i>. It must return either <i>{ok, NewInput}</i> (where
%%% <i>NewInput</i> might be transformed <i>Input</i> - like convertion
%%% from string to integer in case of wtype_integer) or <i>{error, Reason}</i>.
%%% The <i>Reason</i> will be passed to the caller.</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
-module(wtype).

%% API
-export([format/2]).
-export([behaviour_info/1]).

%%====================================================================
%% API
%%====================================================================
%% @hidden
behaviour_info(callbacks) ->
    [{handle_call, 2},
     {validate, 1}];
behaviour_info(_Other) ->
    undefined.

%% @see wpart:format/2
-spec(format/2 :: (string(), term()) -> string()).	     
format(Format0, Value) ->
    case string:tokens(Format0, "[]") of
        ["io"|_] = Type0 ->
            {_Type, Format} = parse_format(lists:flatten(Type0)),
            io_lib:format(Format, Value);
        [Type0] ->
            {Type, Format} = parse_format(Type0),
	    apply(list_to_existing_atom("wtype_" ++ Type),
		  handle_call, [lists:flatten(Format), Value]);
	Error ->
            erlang:error({badarg, Error})
    end.

%%====================================================================
%% Internal functions
%%====================================================================
-spec(parse_format/1 :: (string()) -> {string(), string()}).	     
parse_format(Str) ->
    Offset = string:chr(Str, $(),
    End = string:chr(Str, $)) - Offset,
    if
        Offset == 0; End == 0 -> 
            {Str, []};            
        End == 1 ->
            {string:substr(Str, 1, Offset - 1), []};
        true ->
            {string:substr(Str, 1, Offset - 1), 
             string:substr(Str, Offset + 1, End - 1)}
    end.
