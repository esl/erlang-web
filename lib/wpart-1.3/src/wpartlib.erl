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
%%% File    : wpartlib.erl
%%% Author  : Martin <info@erlang-consulting.com>
%%% Description : 
%%%
%%%-------------------------------------------------------------------
-module(wpartlib).

%% API
-export([select/2,
	 has_attribute/2, 
	 attribute/3,
         attribute/2, 
         format/2,
         eval/1,
         eval_file/2,
         expand_string/1,
	 search/2,
	 search/1,
	 decode_erl/1
	]).

-include_lib("xmerl/include/xmerl.hrl").

%%====================================================================
%% API
%%====================================================================
select(Path, #xmlElement{} = E) ->
    xmerl_xs:select(Path, E).
	    
has_attribute(Path, #xmlElement{} = E) ->
    case xmerl_xs:select(Path, E) of
	[#xmlAttribute{value = Value}] ->
	    Value;
	[] ->
	    false
    end.
    
attribute(Path, #xmlElement{} = E) ->
    case xmerl_xs:select(Path, E) of
	[#xmlAttribute{value = Value}] ->
	    Value;
	[] ->
	    erlang:error({Path, not_set})
    end.

attribute(Path, Default, E) ->
    case xmerl_xs:select(Path, E) of
	[#xmlAttribute{value = Value}] ->
	    Value;
	[] ->
	    Default
    end.

format(Value, E) ->
    case has_attribute("attribute::format", E) of
        false ->
            Value;
        Format ->
            wtype:format(Format, Value)
    end.

search(Value) ->
    search(Value, []).

search(Value, []) ->
    Value;
search(Value, [Token|Tokens]) when is_list(Value) ->
    case lists:keysearch(Token, 1, Value) of
	{value, {Token, NewValue}} ->
	    search(NewValue, Tokens);
	false ->
	    undefined
    end;
search(_,_) ->
    undefined.

eval_file(File, XPath) ->
    eval(xmerl_xs:select(XPath, eptic:read_file(File))).

%%====================================================================
%% Internal functions
%%====================================================================
eval(#xmlElement{nsinfo = {"wpart", Operator}} = E) ->
    Mod = list_to_atom("wpart_" ++ Operator),
    Attr = expand_attributes(E#xmlElement.attributes),
    apply(Mod, handle_call, [E#xmlElement{attributes = Attr}]);
eval(#xmlElement{attributes = Attr, content = []} = E) ->
    E#xmlElement{attributes = expand_attributes(Attr)};
eval(#xmlElement{attributes = Attr} = E) ->
    E#xmlElement{attributes = expand_attributes(Attr),
                 content = eval(E#xmlElement.content)};
eval([H|T]) ->
    [eval(H)|eval(T)]; 
eval(E) ->
    E.

expand_attributes([]) ->
    [];
expand_attributes([#xmlAttribute{namespace = {"wpart", N}, value = V}|T]) ->
    [#xmlAttribute{name = list_to_atom(N), 
		   namespace = [], 
		   nsinfo = [],
		   value = expand_string(V)}|
     expand_attributes(T)];
expand_attributes([A|T]) ->
    [A|expand_attributes(T)].

expand_string(S) ->
    expand_string(S, [], [[]]).

expand_string([], _, [Acc0|Acc]) ->
    lists:flatten(lists:reverse([lists:reverse(Acc0)|Acc]));
expand_string([${, H|T], _, [Acc0|Acc]) ->
    expand_string(T, [H], [lists:reverse(Acc0)|Acc]);
expand_string([$}|T], Op0, Acc) ->
    Value = case string:tokens(lists:reverse(Op0), "[]") of
		[Format, Key] ->
		    [Token|Tokens] = string:tokens(Key,":"),
		    wtype:format("[" ++ Format ++ "]", search(wpart:fget(Token), Tokens));
		[Key] ->
		    [Token|Tokens] = string:tokens(Key,":"),
		    case search(wpart:fget(Token), Tokens) of
			[] -> "[]";
			V when is_list(V) -> V;
			V -> io_lib:format("~p",[V])
		    end
	    end,
    expand_string(T, [], [[], Value |Acc]);
expand_string([H|T], [], [Acc0|Acc]) ->
    expand_string(T, [], [[H|Acc0]|Acc]);
expand_string([H|T], Acc0, Acc) ->
    expand_string(T, [H|Acc0], Acc).

substitute_erl(Erl, From, To) ->
    {ok, NewErl, _} = regexp:gsub(Erl, From, To),
    NewErl.

decode_erl(Code) ->
    lists:foldl(fun({From, To}, Erl) ->
			substitute_erl(Erl, From, To)
		end, Code, [{" lt ", " < "},
			    {" gt ", " > "},
			    {" eq ", " =:= "},
			    {" neq ", " =/= "},
			    {" le ", " =< "},
			    {" ge ", " >= "}]).
