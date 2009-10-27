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
%%% @author Michal Ptaszek <info@erlang-consulting.com>
%%% @doc Module for managing and handling the .tpl template snippets.
%%% @end
%%%-------------------------------------------------------------------
-module(wpart_gen).
-export([load_tpl/2, load_tpl/3, build_html/2, unload_tpl/2]).
-export([tpl_get/1, tpl_get/2]).

%%
%% @spec load_tpl(Name :: term(), PathToTpl :: string()) -> true
%% @doc The same as load_tpl(wpart, Name, PathToTpl).
%% @see load_tpl/3
%%
-spec(load_tpl/2 :: (term(), string()) -> true).	     
load_tpl(Type, Path) ->
    load_tpl(wpart, Type, Path).

%%
%% @spec load_tpl(Namespace :: term(), Name :: term(), PathToTpl :: string()) -> true
%% @doc Loads the template snippet into the memory
%% The key will be a tuple: {Namespace, Name}.
%% 
-spec(load_tpl/3 :: (term(), term(), string()) -> true).
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

%%
%% @spec unload_tpl(Namespace :: term(), Name :: term()) -> true
%% @doc Unloads the selected template from the system
%%
-spec(unload_tpl/2 :: (term(), term()) -> true).	     
unload_tpl(Namespace, Name) ->
    ets:delete(templates, {Namespace, Name}).

%%
%% @spec build_html(Tpl :: list(), Values :: list()) -> Result :: string()
%% @doc Builds a template that is filled with the passed values.
%% There are two possible ways to fill a template. In case of usage
%% the unnamed slots (all of them are &lt;% slot %&gt;) the value list
%% should consists only of the bare strings. Moreover, the Values 
%% list must have the same length as the number of slots.
%% The values will be inserted in the same order as they appear on
%% the Values list. <br/>
%% Otherwise, when other name than "slot" has been used, the Values 
%% list should consists of the tagged tuples: {"name_of_the_slot", "value"}.
%% In this case, the order of the values does not matter. Moreover, 
%% some slots may not be initialized - they will remain empty (so
%% we do not have to provide all values).
%% 
-spec(build_html/2 :: (list(), list()) -> string()).	   
build_html(Tpl, Vals) ->
    if
	is_list(hd(Vals)) ->
	    build_list_html(lists:filter(fun filter_slots/1, Tpl), Vals);
	true ->
	    build_html(Tpl, Vals, [])
    end.

-spec(filter_slots/1 :: (term()) -> bool()).	     
filter_slots({slot, _}) ->
    false;
filter_slots(_) ->
    true.

-spec(build_list_html/2 :: (list(), list()) -> string()).	     
build_list_html([Out | Rest], In) when length(Rest) == length(In) -> 
    concat(in, Rest, In, Out); 
build_list_html(Out, In) -> 
    erlang:error({build_html_failed, Out, In}).

-spec(concat/4 :: (in | out, list(), list(), string()) -> string()).	     
concat(in, [], [], Curr) -> 
    Curr; 
concat(in, Out, [H | Rest], Curr) -> 
    concat(out, Out, Rest, string:concat(Curr, H)); 
concat(out, [H | Rest], In, Curr) -> 
    concat(in, Rest, In, string:concat(Curr, H)). 

-spec(build_html/3 :: (list(), list(), list()) -> string()).	     
build_html([], _, Result) ->
    lists:flatten(lists:reverse(Result));
build_html([{slot, Name} | Rest], Vals, Result) ->
    build_html(Rest, Vals, [proplists:get_value(Name, Vals, "") | Result]);
build_html([String | Rest], Vals, Result) ->
    build_html(Rest, Vals, [String | Result]).

%%
%% @spec tpl_get(Key) -> Tpl
%%               Key = {Namespace, Name} | Name
%%               Tpl = list()
%%               Namespace = term()
%%               Name = term()
%% @doc Loads the template snippet from the memory.
%% The loaded snippet should be used only for passing to the build_html/2 
%% function.
%%
-spec(tpl_get/1 :: (term()) -> list()).	     
tpl_get({Namespace, Name}) ->
    tpl_get(Namespace, Name);
tpl_get(Name) ->
    tpl_get(wpart, Name).

%%
%% @spec tpl_get(Namespace :: term(), Name :: term()) -> Tpl :: list()
%% @doc The same as tpl_get({Namespace, Name}).
%% @see tpl_get/1
%% 
-spec(tpl_get/2 :: (term(), term()) -> list()).	     
tpl_get(Namespace, Name) ->
    [{_, Parts}] = ets:lookup(templates, {Namespace, Name}),
    Parts.

-spec(prepare_named_slots/2 :: (string(), list()) -> list()).	     
prepare_named_slots(String, Slots) ->
    prepare_named_slots(String, 1, length(String), [], Slots).

-spec(prepare_named_slots/5 :: (string(), integer(), integer(), list(), list()) -> list()).	     
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
