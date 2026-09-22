%% taken from: http://user.it.uu.se/~pergu/utf8.erl
%% written by Per Gustafsson http://user.it.uu.se/~pergu 
-module(utf8).
-export([from_binary/1, to_binary/1]).

%% Given a binary of UTF-8 encoded text, return a UTF-32 String
%% (i.e. each element is a unicode code point).
from_binary(Bin) ->
    decode_binary(Bin, []).

decode_binary(<<>>, Str) ->
    {ok, lists:reverse(Str)};
%% 0-7F  0zzzzzzz
decode_binary(<<2#0:1,CodePoint:7,Rest/binary>>, Str) ->
     decode_binary(Rest, [CodePoint|Str]);
%% 110yyyyy 10zzzzzz
decode_binary(<<2#110:3,Y:5,2#10:2,Z:6,Rest/binary>>, Str) ->
    <<CodePoint:16>> = <<0:5,Y:5,Z:6>>,
    decode_binary(Rest, [CodePoint|Str]);
%% 1110xxxx 10yyyyyy 10zzzzzz
decode_binary(<<2#1110:4,X:4,2#10:2,Y:6,2#10:2,Z:6,Rest/binary>>, Str) ->
    <<CodePoint:16>> = <<X:4,Y:6,Z:6>>,
    decode_binary(Rest, [CodePoint|Str]);
%% 11110www 10xxxxxx 10yyyyyy 10zzzzzz
decode_binary(<<2#11110:5,W:3,2#10:2,X:6,
	       2#10:2,Y:6,2#10:2,Z:6,Rest/binary>>, Str) ->
    <<CodePoint:24>> = <<0:3,W:3,X:6,Y:6,Z:6>>,
    decode_binary(Rest, [CodePoint|Str]).

%% Given a list of unicode code points, return a binary of UTF-8
%% encoded text.
to_binary(Str) ->
    encode_utf32(Str, []).

encode_utf32([], Utf8) ->
    {ok, list_to_binary(lists:reverse(Utf8))};
 %% 0-7F  0zzzzzzz -> 0zzzzzzz
encode_utf32([U32|Str], Utf8) when U32 < 16#80 ->
    encode_utf32(Str, [U32|Utf8]);
%% 80-7FF yyy yyzzzzzz -> 110yyyyy 10zzzzzz
encode_utf32([U32|Str], Utf8) when U32 < 16#800 ->
    <<_:5,Y:5,Z:6>> = <<U32:16>>,
    encode_utf32(Str, [<<2#110:3,Y:5,2#10:2,Z:6>>|Utf8]);
%% 800-FFFF xxxxyyyy yyzzzzzz -> 1110xxxx 10yyyyyy 10zzzzzz
encode_utf32([U32|Str], Utf8) when U32 < 16#10000 ->
    <<X:4,Y:6,Z:6>> = <<U32:16>>,
    encode_utf32(Str, [<<2#1110:4,X:4,2#10:2,Y:6,2#10:2,Z:6>>|Utf8]);
%% 10000-10FFFF wwwxx xxxxyyyy yyzzzzzz -> 11110www 10xxxxxx 10yyyyyy 10zzzzzz
encode_utf32([U32|Str], Utf8) when U32 < 16#110000 ->
    <<0:3,W:3,X:6,Y:6,Z:6>> = <<U32:24>>,
    encode_utf32(Str, [<<2#11110:5,W:3,2#10:2,X:6,
			 2#10:2,Y:6,2#10:2,Z:6>>|Utf8]).


