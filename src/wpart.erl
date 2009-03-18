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
%%% @author Martin Carlson <martin@erlang-consulting.com>
%%% @doc 
%%% 
%%% @end
%%%-------------------------------------------------------------------
-module(wpart).

%% API
-export([
	 fget/1, 
	 fset/2, 
	 select/2, 
	 has_attribute/2, 
	 has_attribute/3,
	 eval/1,
	 format/2,
	 eval_file/2,
         expand_string/1,
	 search/2,
	 search/1,
	 finsert/2,
	 fdelete/1
	]).
-export([behaviour_info/1]).

-include_lib("xmerl/include/xmerl.hrl").

%%====================================================================
%% API
%%====================================================================
%% @hidden
behaviour_info(callbacks) ->
    [{handle_call,1},
     {load_tpl,0},
     {build_html_tag,4}];
behaviour_info(_Other) ->
    undefined.

fget(Key0) ->
    case string:tokens(Key0, ":") of
	[List, Key] ->
	    eptic:fget(List, Key);
	[Key] ->
	    eptic:fget(Key)
    end.

fset(Key0, Value) ->
    case string:tokens(Key0, ":") of
	[List, Key] ->
	    eptic:fset(List, Key, Value);
	[Key] ->
	    eptic:fset(Key, Value)
    end.
    
select(Path, E) ->
    wpartlib:select(Path, E).

has_attribute(Path, #xmlElement{} = E) ->
    wpartlib:has_attribute(Path, E).

has_attribute(Path, Default, XmlElement) ->
    wpartlib:attribute(Path, Default, XmlElement).

eval(E) ->
    wpartlib:eval(E).

format(Value, E) ->
    wpartlib:format(Value, E).

eval_file(File, XPath) ->
    wpartlib:eval_file(File, XPath).

expand_string(S) ->
    wpartlib:expand_string(S).

search(Value, Tokens) ->
    wpartlib:search(Value, Tokens).

search(Value) ->
    wpartlib:search(Value).

finsert(Key0, Val) ->
    [List, Key] = string:tokens(Key0, ":"),
    eptic:finsert(List, Key, Val).

fdelete(Key0) ->	
    case string:tokens(Key0, ":") of
	[List, Key] ->
	    eptic:fdelete(List, Key);
	[Key] ->
	    eptic:fdelete(Key)
    end.

%%====================================================================
%% Internal functions
%%====================================================================
