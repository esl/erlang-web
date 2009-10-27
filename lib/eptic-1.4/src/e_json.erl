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
%%% @copyright (C) 2006, Erlang Training & Consulting Ltd.
%%% @author Martin Carlson <martin@erlang-consulting.com>
%%% @doc 
%%% JSON handling functions for exporting json terms
%%% @end
%%%-------------------------------------------------------------------
-module(e_json).

%% API
-export([encode/1, decode/1]).

%%====================================================================
%% API
%%====================================================================
%%-------------------------------------------------------------------
%% @spec encode(term()) -> json_type()
%% @type json_type() = string()
%% @doc Encodes the string to the JSON format.
%% This implementation handles dicts correctly
%% Other [H|T] where H is a tuple might get problems
%% since they are treated as tag lists
%% @todo fix tag list handling
%% @end
%%-------------------------------------------------------------------
-spec(encode/1 :: (term()) -> string()).	     
encode(true) ->
    "true";
encode(false) ->
    "false";
encode([]) ->
    "null";
encode(Int) when is_integer(Int) ->
    integer_to_list(Int);
encode(Float) when is_float(Float) ->
    float_to_list(Float);
encode([Tuple|T]) when is_tuple(Tuple), size(Tuple) == 2 ->
    tuple_to_object([Tuple|T], []);
encode(Tuple) when is_tuple(Tuple) ->
    list_to_json_array(tuple_to_list(Tuple), []);
encode([H|T]) when not is_integer(H) ->
    encode({"__list", list_to_json_array([H|T], [])});
encode(List) when is_list(List) ->
    string_to_json_string(List, []);
encode(Atom) when is_atom(Atom) ->
    encode({"__atom", atom_to_list(Atom)});
encode(Binary) when is_binary(Binary)-> 
    encode({"__binary", list_to_tuple(binary_to_list(Binary))}).
	
%%-------------------------------------------------------------------
%% @spec decode(string()) -> {ok, term()}
%% @type json_type() = string()
%% @doc 
%% Decodes the input string from JSON format to the Erlang term.
%% @end
%%-------------------------------------------------------------------
-spec(decode/1 :: (string()) -> {ok, term()}).	     
decode(String) ->
    {ok, scan(String, [])}.

%%-------------------------------------------------------------------
%%% Internal functions
%%-------------------------------------------------------------------
-spec(tuple_to_object/2 :: (list(tuple()), list()) -> string()).	     
tuple_to_object([], Acc) ->
    [${|lists:flatten(lists:reverse([$}|Acc]))];
tuple_to_object([{Key, Value}], Acc) when is_atom(Key) ->
    Item = [encode(atom_to_list(Key)), ":", encode(Value)],
    tuple_to_object([], [Item|Acc]);
tuple_to_object([{Key, Value}|T], Acc) when is_atom(Key) ->
    Item = [encode(atom_to_list(Key)), ":", encode(Value), ","],
    tuple_to_object(T, [Item|Acc]);
tuple_to_object([{Key, Value}|T], []) ->
    tuple_to_object(T, [[encode(Key), ":", encode(Value), ","]]);
tuple_to_object([{Key, Value}|T], Acc) ->
    Item = [encode(Key), ":", encode(Value), ","],
    tuple_to_object(T, [Item|Acc]). 

-spec(list_to_json_array/2 :: (list(), string()) -> string()).	     
list_to_json_array([], Acc) ->
    lists:flatten([$[|lists:reverse([$]|Acc])]);
list_to_json_array([Item], Acc) ->
    list_to_json_array([], [encode(Item)|Acc]);
list_to_json_array([Item|T], Acc) ->
    list_to_json_array(T, [$,, encode(Item)|Acc]).

-spec(string_to_json_string/2 :: (string(), string()) -> string()).	     
string_to_json_string([], Acc) ->
    [34|lists:reverse([34|Acc])]; % $"
string_to_json_string([$\n|T], Acc) ->
    string_to_json_string(T, [$\n,$\\|Acc]);
string_to_json_string([$\r|T], Acc) ->
    string_to_json_string(T, [$\r,$\\|Acc]);
string_to_json_string([$\t|T], Acc) ->
    string_to_json_string(T, [$\t,$\\|Acc]);
string_to_json_string([$\b|T], Acc) ->
    string_to_json_string(T, [$\b,$\\|Acc]);
string_to_json_string([$\\|T], Acc) ->
    string_to_json_string(T, [$\\,$\\|Acc]);
string_to_json_string([$\/|T], Acc) ->
    string_to_json_string(T, [$\/,$\\|Acc]);
string_to_json_string([34|T], Acc) -> % $"
    string_to_json_string(T, [34,$\\|Acc]);
string_to_json_string([$ |T], Acc) ->
    string_to_json_string(T, [$ |Acc]);
string_to_json_string([H|T], Acc) ->
    string_to_json_string(T, [H|Acc]).

-spec(scan/2 :: (string(), list(term())) -> list(term()) | {list(term()), string()}).
scan([], Acc) ->
    lists:reverse(Acc);
scan([$ |T], Acc) ->
    scan(T, Acc);
scan([123|T], Acc) ->  %% ${
    {TagList, T0} = scan_taglist(T, []),
    scan(T0, [TagList|Acc]);
scan([91|T], Acc) -> %% $[
    {List, T0} = scan_list(T, []),
    case list_to_tuple(List) of
	{"__atom", Atom} ->
	    scan(T0, [list_to_atom(Atom) | Acc]);
	{"__list", NestedList} ->
	    [NestedListEncoded] = scan(NestedList, []),
	    scan(T0, [tuple_to_list(NestedListEncoded) | Acc]);
	{"__binary", Binary} ->
	    scan(T0, [list_to_binary(tuple_to_list(Binary)) | Acc]);
	Else ->
	    scan(T0, [Else | Acc])
    end;
scan([34|T], Acc) ->
    {String, T0} = scan_string(T, []),
    scan(T0, [String|Acc]);
scan([N|T], Acc) when N >= $0, N =< $9; N == $- ->
    {Number, T0} = scan_number([N|T], [], false),
    scan(T0, [Number|Acc]);
scan("true" ++ T, Acc) ->
    scan(T, [true|Acc]);
scan("false" ++ T, Acc) ->
    scan(T, [false|Acc]);
scan("null" ++ T, Acc) ->
    scan(T, [[]|Acc]);
scan("\r\n" ++ T, Acc) ->
    scan(T, Acc);
scan(T, Acc) ->
    {lists:reverse(Acc), T}.

-spec(scan_taglist/2 :: (string(), list(tuple())) -> {list(tuple()), string()}).	     
scan_taglist([$}|T], Acc) ->
    {lists:reverse(Acc), T};
scan_taglist([$:|T], [{Key, _}|Acc]) ->  
    {[Value], T0} = scan(T, []),
    scan_taglist(T0, [{Key, Value}|Acc]);
scan_taglist([$,|T], Acc) ->
    {[Key], T0} = scan(T, []),
    scan_taglist(T0, [{list_to_atom(Key), []}|Acc]);
scan_taglist(TagList, Acc) ->
    {[Key], T0} = scan(TagList, []),
    scan_taglist(T0, [{list_to_atom(Key), []}|Acc]).

-spec(scan_list/2 :: (string(), list(term())) -> {list(term()), string()}).	     
scan_list([$]|T], Acc) ->
    {lists:reverse(Acc), T};
scan_list([$,|T], Acc) ->
    {[Item], T0} = scan(T, []),
    scan_list(T0, [Item|Acc]);
scan_list([H|T], Acc) ->
    {[Item], T0} = scan([H|T], []),
    scan_list(T0, [Item|Acc]).

-spec(scan_string/2 :: (string(), string()) -> {string(), string()}).
scan_string([$\\, $u, A1, A2, A3, A4|T], Acc) ->
    scan_string(T, [binary_to_list(unicode:characters_to_binary(
				     [erlang:list_to_integer([A1, A2, A3, A4], 16)], 
				     unicode)) | Acc]);
scan_string([$\\,C|T], Acc) ->
    scan_string(T, [esc(C)|Acc]);
scan_string([34|T], Acc) ->
    {lists:reverse(Acc), T};
scan_string([H|T], Acc) ->
    scan_string(T,[H|Acc]).

-spec(scan_number/3 :: (string(), string(), bool()) -> {integer() | float(), string()}).
scan_number([$-|T], Acc, Float) ->
    scan_number(T, [$-|Acc], Float);
scan_number([$.|T], Acc, _) ->
    scan_number(T, [$.|Acc], true);
scan_number([$e|T], Acc, _) ->
    scan_number(T, [$e|Acc], true);
scan_number([$E|T], Acc, _) ->
    scan_number(T, [$E|Acc], true);
scan_number([$+|T], Acc, _) ->
    scan_number(T, [$+|Acc], true);
scan_number([N|T], Acc, Float) when N >= $0, N =< $9 ->
    scan_number(T, [N|Acc], Float);
scan_number(T, Acc, false) ->
    {list_to_integer(lists:reverse(Acc)), T};
scan_number(T, Acc, true) ->
    {list_to_float(lists:reverse(Acc)), T}.

-spec(esc/1 :: (char()) -> char()).	     
esc($\\) -> 
    $\\;
esc($b) -> 
    $\b;
esc($f) -> 
    $\f;
esc($n) -> 
    $\n;
esc($r) -> 
    $\r;
esc($t) -> 
    $\t;
esc(C) ->
    C.
