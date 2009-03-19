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
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc Module holding the helper functions for wpart application.
%%% @end
%%%-------------------------------------------------------------------
-module(wpart_utils).
-export([xml2proplist/1, proplist2html/1, normalize_html_attrs/1]).

%% taken from Yaws server
-export([url_encode/1, url_decode/1, integer_to_hex/1, hex_to_integer/1]).

-include_lib("xmerl/include/xmerl.hrl").

%%
%% @spec url_encode(URL :: string()) -> EncodedURL :: string()
%% @doc Url-encodes a string. 
%% All URLs in HTML documents must be URL encoded. <br/>
%% This function has been taken from <i>Yaws</i> web server.
%% 
-spec(url_encode/1 :: (string()) -> string()).	     
url_encode([H|T]) ->
    if
        H >= $a, $z >= H ->
            [H|url_encode(T)];
        H >= $A, $Z >= H ->
            [H|url_encode(T)];
        H >= $0, $9 >= H ->
            [H|url_encode(T)];
        H == $_; H == $.; H == $-; H == $/; H == $: -> % FIXME: more..
            [H|url_encode(T)];
        true ->
            case integer_to_hex(H) of
                [X, Y] ->
                    [37, X, Y | url_encode(T)];
                [X] ->
                    [37, $0, X | url_encode(T)]
            end
     end;

url_encode([]) ->
    [].

%%
%% @spec url_decode(URL :: string()) -> DecodedURL :: string()
%% @doc Decode url-encoded string.
%% A URL  encoded  string  is  a  string
%% where  all  alfa  numeric characters and the the character _ are
%% preserved and all other characters are encode as "%XY"  where  X
%% and  Y  are the hex values of the least respective most 
%% significant 4 bits in the 8 bit character.<br/>
%% This function has been taken from <i>Yaws</i> web server.
%%
-spec(url_decode/1 :: (string()) -> string()).	     
url_decode([37, Hi, Lo | Tail]) ->
    Hex = hex_to_integer([Hi, Lo]),
    [Hex | url_decode(Tail)];
url_decode([$?|T]) ->
    %% Don't decode the query string here, that is parsed separately.
    [$?|T];
url_decode([H|T]) when integer(H) ->
    [H |url_decode(T)];
url_decode([]) ->
    [];
%% deep lists
url_decode([H|T]) when list(H) ->
    [url_decode(H) | url_decode(T)].

%%
%% @spec integer_to_hex(Integer :: integer()) -> Hex :: string()
%% @doc Converts the integer to the hex format.
%%
-spec(integer_to_hex/1 :: (integer()) -> string()).	     
integer_to_hex(I) ->
    case catch erlang:integer_to_list(I, 16) of
        {'EXIT', _} ->
            old_integer_to_hex(I);
        Int ->
            Int
    end.

-spec(old_integer_to_hex/1 :: (integer()) -> string()).
old_integer_to_hex(I) when I<10 ->
    integer_to_list(I);
old_integer_to_hex(I) when I<16 ->
    [I-10+$A];
old_integer_to_hex(I) when I>=16 ->
    N = trunc(I/16),
    old_integer_to_hex(N) ++ old_integer_to_hex(I rem 16).

%%
%% @spec hex_to_integer(Hex :: string()) -> Integer :: integer()
%% @doc Converts the hex to the integer format.
%%
-spec(hex_to_integer/1 :: (string()) -> integer()).	     
hex_to_integer(Hex) ->
    erlang:list_to_integer(Hex, 16).
%% 
%% @spec xml2proplist(XMLAttributes :: list(tuple())) -> AttributesProplist :: list(tuple())
%% @see wpart:xml2proplist/1
%%
-spec(xml2proplist/1 :: (list(tuple())) -> list(tuple())).	     
xml2proplist(XML) ->
    lists:foldl(fun xml2proplist/2, [], XML).

-spec(xml2proplist/2 :: (tuple(), list(tuple())) -> list(tuple())).	     
xml2proplist(#xmlAttribute{name = Name, value = Value}, Proplist) ->
    [{atom_to_list(Name), Value} | Proplist].

%%
%% @spec proplist2html(AttributesProplist :: list(tuple())) -> HTML :: string()
%% @see wpart:proplist2html/1
%%
-spec(proplist2html/1 :: (list(tuple())) -> string()).	      
proplist2html(Proplist) ->
    string:join(lists:map(fun proplist2html1/1, Proplist), " ").

-spec(proplist2html1/1 :: ({string(), string()}) -> string()).	     
proplist2html1({Name, Value}) ->
    Name ++ "=\"" ++ Value ++ "\"".

%%
%% @spec normalize_html_attrs(AttributesProplist :: list(tuple())) -> NormalizedAttributesProplist :: list(tuple())
%% @see wpart:normalize_html_attrs/1
%%
-spec(normalize_html_attrs/1 :: (list(tuple())) -> list(tuple())).	     
normalize_html_attrs(Proplist) ->
    lists:map(fun normalize_html_attrs1/1, Proplist).

-spec(normalize_html_attrs1/1 :: (tuple()) -> tuple()).	     
normalize_html_attrs1({Key, Value}) when is_atom(Key), is_atom(Value) ->
    {atom_to_list(Key), atom_to_list(Value)};
normalize_html_attrs1({Key, Value}) when is_atom(Key), is_integer(Value) ->
    {atom_to_list(Key), integer_to_list(Value)};
normalize_html_attrs1({Key, Value}) when is_atom(Key) ->
    {atom_to_list(Key), Value};
normalize_html_attrs1({Key, Value}) when is_atom(Value) ->
    {Key, atom_to_list(Value)};
normalize_html_attrs1({Key, Value}) when is_integer(Value) ->
    {Key, integer_to_list(Value)};
normalize_html_attrs1(Else) ->
    Else.
