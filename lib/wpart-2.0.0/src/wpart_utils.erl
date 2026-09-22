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
-export([xml2proplist/1, proplist2html/1, getValue/1, normalize_html_attrs/1]).
-export([find_pk/1, pk2string/1,
	 term2string/3, term2string/2,
	 string2term/3, string2term/2]).

%% taken from Yaws server
-export([url_encode/1, url_decode/1, integer_to_hex/1, hex_to_integer/1]).

-include_lib("xmerl/include/xmerl.hrl").

%%
%% @spec url_encode(URL :: string()) -> EncodedURL :: string()
%% @doc Url-encodes a string.
%% All URLs in HTML documents must be URL encoded. <br/>
%%
%% This function escapes everything except url-safe characters
%% [-._0-9a-zA-Z] AND SLASHES, which sould be very DWIM-y
%% It should be consistent with Python3's urllib.parse.quote
%%
-spec(url_encode/1 :: (string()) -> string()).
url_encode(Str) when is_list(Str) ->
    S = xmerl_ucs:to_utf8(Str),
    url_encode_char(lists:reverse(S), []).

url_encode_char([X | T], Acc) when X >= $0, X =< $9 ->
    url_encode_char(T, [X | Acc]);
url_encode_char([X | T], Acc) when X >= $a, X =< $z ->
    url_encode_char(T, [X | Acc]);
url_encode_char([X | T], Acc) when X >= $A, X =< $Z ->
    url_encode_char(T, [X | Acc]);
url_encode_char([X | T], Acc) when X == $-; X == $_; X == $. ->
url_encode_char(T, [X | Acc]);
url_encode_char([X | T], Acc) when X == $/ -> % this is DWIM bit
    url_encode_char(T, [X | Acc]);
url_encode_char([X | T], Acc) ->
    url_encode_char(T, [$%, d2h(X bsr 4), d2h(X band 16#0f)
            | Acc]);
url_encode_char([], Acc) ->
    Acc.

d2h(N) when N<10 -> N+$0;
d2h(N) -> N+$a-10.

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
url_decode([H|T]) when is_integer(H) ->
    [H |url_decode(T)];
url_decode([]) ->
    [];
%% deep lists
url_decode([H|T]) when is_list(H) ->
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
%% @spec getValue(AttributesProplist :: list(tuple())) -> HTML :: string()
%% @see wpart:getValue/1
%%
-spec(getValue/1 :: (list(tuple())) -> any()).
getValue(Proplist) ->
    case proplists:get_value("valueVar", Proplist) of
        undefined -> proplists:get_value("value", Proplist, "");
        Var -> case wpart:fget(Var) of
                   undefined -> "";
                   Some -> Some
               end
    end.
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

%%
%% @spec string2term(Type :: atom(), String :: string()) -> Result
%%                   Result = {ok, Term :: any()} | {error, Reason :: term()}
%%
%% @doc The same as <i>string2term(Type, String, [])</i>.
%%
-spec(string2term/2 :: (atom(), string()) -> {ok, any()} | {error, any()}).
string2term(Type, String) ->
    string2term(Type, String, []).

%%
%% @spec string2term(Type, String, Parameters) -> Result
%%                   Type = atom()
%%                   String = string()
%%                   Parameters = list()
%%                   Result = {ok, Term :: any()} | {error, Reason :: term()}
%%
%% @doc Converts the string to the particular type.
%% <p>Type-specific validators are used for casting the string representation
%% of the term (e.g. taken from the HTML form) to the Erlang internal one.</p>
%% <p>The <i>Parameters</i> should be a list of constraints such as taken from
%% the Type_types record.</p>
%%
-spec(string2term/3 :: (atom(), string(), list()) -> {ok, any()} | {error, any()}).
string2term(Type, String, Params0) ->
    Params = proplists:delete(private, Params0),
    (list_to_atom("wtype_" ++ atom_to_list(Type))):validate({Params, String}).

%%
%% @spec term2string(Type :: atom(), Term :: term()) -> String :: string()
%%
%% @doc The same as <i>term2string(Type, Term, "")</i>.
%%
-spec(term2string/2 :: (atom(), term()) -> string()).
term2string(Type, Term) ->
    term2string(Type, Term, "").

%%
%% @spec term2string(Type :: atom(), Term :: term(), Format :: string()) -> String :: string()
%%
%% @doc Converts the Term to the string representation using the Format.
%% The type-specific handle_call is used to do the conversion.
%%
-spec(term2string/3 :: (atom(), term(), string()) -> string()).
term2string(Type, Term, Format) ->
    (list_to_atom("wtype_" ++ atom_to_list(Type))):handle_call(Format, Term).

%%
%% @spec pk2string(Record :: tuple()) -> Result
%%                 Result = {PKPosition, PKString}
%%                 PKPosition = integer()
%%                 PKString = string()
%%
%% @doc Converts the primary key field of the record to its string representation
%% The returning value is a tuple consisting of the primary key
%% position in the record (note that first element of the tuple is
%% the record name) and the converted primary key.
%%
-spec(pk2string/1 :: (tuple()) -> {integer(), string()}).
pk2string(Record) ->
    TypeS = atom_to_list(element(1, Record)),
    TypesDesc = tl(tuple_to_list((list_to_atom("wtype_" ++ TypeS)):
				 get_record_info(list_to_atom(TypeS ++ "_types")))),
    {PKPos, {PKType, PKDesc}} = case find_pk(TypesDesc, 1) of
				    no_pk ->
					{1, hd(TypesDesc)};
				    Else ->
					Else
				end,

    {PKPos, term2string(PKType, element(PKPos+1, Record),
			proplists:get_value(format, PKDesc, ""))}.

%%
%% @spec find_pk(TypeOptions :: list()) -> Result
%%               Result = no_pk | {PKPosition :: integer(), PKOptions :: list()}
%%
%% @doc Search the primary key in the list of the type's options.
%% TypeOptions == tl(get_record_info(Type_types)).
%%
-spec(find_pk/1 :: (list({atom(), list()})) -> {integer(), {atom(), list()}} | no_pk).
find_pk(TypeDecl) ->
    find_pk(TypeDecl, 1).

-spec(find_pk/2 :: (list({atom(), list()}), integer()) -> {integer(), {atom(), list()}} | no_pk).
find_pk([{_, Options} = TypeDecl | Rest], Pos) ->
    case check_for_pk(Options) of
	true ->
	    {Pos, TypeDecl};
	false ->
	    find_pk(Rest, Pos+1)
    end;
find_pk([], _) ->
    no_pk.

-spec(check_for_pk/1 :: (list()) -> boolean()).
check_for_pk([primary_key | _]) ->
    true;
check_for_pk([{primary_key} | _]) ->
    true;
check_for_pk([_ | Rest]) ->
    check_for_pk(Rest);
check_for_pk([]) ->
    false.
