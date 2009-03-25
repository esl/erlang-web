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
%%% @author Michal Ptaszek <info@erlang-consulting.com>
%%% @doc 
%%% @end
%%%-------------------------------------------------------------------
-module(wpart_gen).
-export([load_tpl/2, load_tpl/3, build_html/2, unload_tpl/2]).
-export([tpl_get/1, tpl_get/2]).

load_tpl(Type, Path) ->
    load_tpl(wpart, Type, Path).

load_tpl(Namespace, Type, Path) ->
    {ok, Binary} = file:read_file(Path),
    Content = binary_to_list(Binary),

    Slots = case re:run(Content, "<% *([A-Za-z_][A-Za-z0-9_]*) *%>", [ungreedy, global, {capture, all}]) of
		{match, SlotsList} ->
		    prepare_named_slots(Content, SlotsList);
		_ ->
		    [Content]
	    end,
    
    ets:insert(templates, {{Namespace, Type}, Slots}).

unload_tpl(Namespace, Name) ->
    ets:delete(templates, {Namespace, Name}).

build_html(Tpl, Vals) ->
    if
	is_list(hd(Vals)) ->
	    build_list_html(lists:filter(fun filter_slots/1, Tpl), Vals);
	true ->
	    build_html(Tpl, Vals, [])
    end.

filter_slots({slot, _}) ->
    false;
filter_slots(_) ->
    true.

build_list_html([Out | Rest], In) when length(Rest) == length(In) -> 
    concat(in, Rest, In, Out); 
build_list_html(Out, In) -> 
    erlang:error({build_html_failed, Out, In}).

concat(in, [], [], Curr) -> 
    Curr; 
concat(in, Out, [H | Rest], Curr) -> 
    concat(out, Out, Rest, string:concat(Curr, H)); 
concat(out, [H | Rest], In, Curr) -> 
    concat(in, Rest, In, string:concat(Curr, H)). 

build_html([], _, Result) ->
    lists:flatten(lists:reverse(Result));
build_html([{slot, Name} | Rest], Vals, Result) ->
    build_html(Rest, Vals, [proplists:get_value(Name, Vals, "") | Result]);
build_html([String | Rest], Vals, Result) ->
    build_html(Rest, Vals, [String | Result]).

tpl_get({Namespace, Name}) ->
    tpl_get(Namespace, Name);
tpl_get(Name) ->
    tpl_get(wpart, Name).

tpl_get(Namespace, Name) ->
    [{_, Parts}] = ets:lookup(templates, {Namespace, Name}),
    Parts.

prepare_named_slots(String, Slots) ->
    prepare_named_slots(String, 1, length(String), [], Slots).

prepare_named_slots(_, Len, Len, Ready, []) ->
    lists:reverse(Ready);
prepare_named_slots(String, Pos, _, Ready, []) ->
    lists:reverse([string:substr(String, Pos) | Ready]);
prepare_named_slots(String, Pos, Len, Ready, [[{FStart, FLen}, {NStart, NLen}] | Rest]) ->
    SlotName = string:substr(String, NStart+1, NLen),
    StringBeg = string:substr(String, Pos, FStart-(Pos-1)),
    
    prepare_named_slots(String, FStart+FLen+1, Len, 
			[{slot, SlotName}, StringBeg | Ready], 
			Rest).
