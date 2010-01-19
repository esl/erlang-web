%% taken from: http://user.it.uu.se/~pergu/utf8.erl
%% written by Per Gustafsson http://user.it.uu.se/~pergu 

-module(utf8_api).
-export([ulength/1, ustring/1]).

%% @doc takes regular string encoded in utf.
ulength(String) ->
    {Res, List} = utf8:from_binary(list_to_binary(String)),
    if 
	Res == ok -> length(List);
	true -> Res
    end.

ustring(RawString) ->
    case (catch utf8:from_binary(list_to_binary(RawString))) of
	{ok, String} ->
	    String;
	_ ->
	    RawString
    end.
