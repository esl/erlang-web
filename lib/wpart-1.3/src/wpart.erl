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
%%% @doc An interface to the Wpart application.
%%% Only this module should be used to access the Wpart application
%%% specific module, because the internal implementation of the
%%% particular functions could be changed in the future.<br/>
%%% Wpart defines also a behaviour - the set of the functions that
%%% each of the wpart should implement:
%%% <ul>
%%% <li><i>handle_call(XmlElement)</i> - the function that should return
%%% a Xmerl structure that will replace the specific wpart:something tag.
%%% </li>
%%% <li><i>load_tpl()</i> - if wpart uses the tpls (what is strongly recommended
%%% for clarity and/or performance reasons) they should be prepared before
%%% using - it is done during the system start. <i>load_tpl/0</i>
%%% should insert the prepared tpl into the <i>templates</i> ETS table.
%%% Moreover it is recommended using the <i>wpart_gen</i> module - it
%%% implements all necessary functions that handle the tpl splitting,
%%% building and loading.</li>
%%% <li><i>build_html_tag/4</i> - function responsible for building
%%% part of the form during the automatic form building phase.</li>
%%% </ul>
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
	 xml2proplist/1,
	 proplist2html/1,
         getValue/1,
	 normalize_html_attrs/1,
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
     {build_html_tag,3}];
behaviour_info(_Other) ->
    undefined.

%%
%% @spec fget(Key :: string()) -> Result :: term()
%% @doc Retrives the value from the request dictionary.
%% The ":" character in key separates the levels of the
%% dictionaries: the first part is used to retrive the main
%% one, the second is used for obtaining the value from the
%% dictionary got in the prior step. <br/>
%% The example of the usage is accessing the session variables:
%% by calling <i>wpart:fget("session:user")</i> we are accesing
%% at first the dictionary <i>session</i> and then the value
%% <i>user</i> within it.
%% @end
%%
-spec(fget/1 :: (string()) -> term()).
fget(Key0) ->
    case string:tokens(Key0, ":") of
	[List, Key] ->
	    eptic:fget(List, Key);
	[Key] ->
	    eptic:fget(Key)
    end.

%%
%% @spec fset(Key :: string(), Value :: term()) -> none()
%% @doc Sets the value in the request dictionary.
%% The meaning of the key is analogous as in <i>fget/1</i>
%% function.
%% @end
%% @see fget/1
%%
-spec(fset/2 :: (string(), any()) -> any()).
fset(Key0, Value) ->
    case string:tokens(Key0, ":") of
	[List, Key] ->
	    eptic:fset(List, Key, Value);
	[Key] ->
	    eptic:fset(Key, Value)
    end.

%%
%% @spec select(XPath :: string(), XmlStructure :: tuple()) -> Result :: tuple()
%% @doc Extracts the nodes from the xml tree according to <i>XPath</i>.
%%
-spec(select/2 :: (string(), tuple()) -> term()).
select(Path, E) ->
    wpartlib:select(Path, E).

%%
%% @spec has_attribute(XPath :: string(), XmlStructure :: tuple()) ->
%%   AttrVal :: string() | false
%% @doc Extracts the attribute value according to the <i>XPath</i>.
%% If no attribute is found, the <i>false</i> atom is returned.<br/>
%% The example of the usage: <i>wpart:has_attribute("attribute::name", E)</i>.
%%
-spec(has_attribute/2 :: (string(), tuple()) -> false | string()).
has_attribute(Path, #xmlElement{} = E) ->
    wpartlib:has_attribute(Path, E).

%%
%% @spec has_attribute(XPath :: string(), DefaultVal :: term(), XmlStructure :: tuple()) ->
%%   AttrVal :: string() | DefaultVal
%% @doc Extracts the attribute value according to the <i>XPath</i>.
%% If no attribute is found, <i>DefaultVal</i> is returned.
%%
-spec(has_attribute/3 :: (string(), term(), tuple()) -> false | term()).
has_attribute(Path, Default, XmlElement) ->
    wpartlib:attribute(Path, Default, XmlElement).

%%
%% @spec eval(XMLElement :: list(tuple()) | tuple()) -> list(string()) | string()
%% @doc Expands the XML elements.
%% If the element's namespace is set to <i>wpart</i> - it will be expanded.
%%
-spec(eval/1 :: (tuple() | list(tuple())) -> list(string()) | string()).
eval(E) ->
    wpartlib:eval(E).

%%
%% @spec format(Value :: term(), XMLStructure :: tuple()) -> FormattedValue :: term()
%% @doc Formats the <i>Value</i>.
%% If the <i>XMLStructure</i> has the attribute <i>format</i>
%% its value is passed to the formatter. In other case, the passed
%% value is returned.
%%
-spec(format/2 :: (term(), tuple()) -> term()).
format(Value, E) ->
    wpartlib:format(Value, E).

%%
%% @spec eval_file(Filename :: string(), XPath :: string()) -> Result :: list(string()) | string()
%% @doc Evaluates the part specified by <i>XPath</i> of the file.
%% The behaviour is the same as reading the file, applying <i>select/2</i>
%% and calling <i>eval/1</i> by hand.
%%
-spec(eval_file/2 :: (string(), string()) -> list(string()) | string()).
eval_file(File, XPath) ->
    wpartlib:eval_file(File, XPath).

%%
%% @spec expand_string(String :: string()) -> ExpandedString :: string()
%% @doc Expands the string basing on the formatting conventions.
%% The text between the curly brackets ({}) will be replaced with
%% the corresponding value fetched from the request dictionary
%% (the key to the value is the text itself). It is also possible to
%% specify formatter for the value.<br/>
%% For example:
%% <ul>
%% <li>"{this_is_a_key}" will be changed to the value fetched
%% from the request dictionary, stored under the key "this_is_the_key".</li>
%% <li>"{[my_formatter(formatting_rule)]this_is_a_key}" - will retrive the
%% value from the request dictionary (key = "this_is_a_key") and format that value
%% by calling <i>wtype_my_formatter:handle_call("formatting_rule", Value)</i></li>
%% </ul>
%%
-spec(expand_string/1 :: (string()) -> string()).
expand_string(S) ->
    wpartlib:expand_string(S).

search(Value, Tokens) ->
    wpartlib:search(Value, Tokens).

%% @hidden
%% @FIXME - it simply returnes the same value - something is probably wrong ;)
-spec(search/1 :: (term()) -> term()).
search(Value) ->
    wpartlib:search(Value).

%%
%% @spec finsert(Key :: string(), Value :: term()) -> any()
%% @doc Inserts the value to the request dictionary.
%% The passed key must contain a colon (:) taht separates
%% the two level ditionaries.
%%
-spec(finsert/2 :: (string(), term()) -> any()).
finsert(Key0, Val) ->
    [List, Key] = string:tokens(Key0, ":"),
    eptic:finsert(List, Key, Val).

%%
%% @spec xml2proplist(XML :: list(tuple())) -> Proplist :: list(tuple())
%% @doc Transforms the list of the attributes stored as #xmlAttribute records
%% to the proplist format: [{"attribute_name", "attribute_value"}].
%%
-spec(xml2proplist/1 :: (list(tuple())) -> list(tuple())).
xml2proplist(XML) ->
    wpart_utils:xml2proplist(XML).

%%
%% @spec proplist2html(AttributesProplist :: list(tuple())) -> HTML :: string()
%% @doc Transforms a proplist of attributes to the string that could be inserted into the HTML tag.
%% This functions allows to insert the proplist of attributes straight to the
%% tpl file. <br/>
%% Note that the proplist must be in a proper format (both keys and values
%% must be strings),
%% @see normalize_html_attrs/1
%%
-spec(proplist2html/1 :: (list(tuple())) -> string()).
proplist2html(Proplist) ->
    wpart_utils:proplist2html(Proplist).

-spec(getValue/1 :: (list(tuple())) -> any()).
getValue(Proplist) ->
    wpart_utils:getValue(Proplist).
%%
%% @spec normalize_html_attrs(AttributesProplist :: list(tuple())) -> NormalizedProplist :: list(tuple())
%% @doc Converts all the attributes to the one - proplist2html compatible format.
%% Each key and value are transformed to the strings, so it is possible to
%% use that type of list in proplist2html function.
%%
-spec(normalize_html_attrs/1 :: (list(tuple())) -> list(tuple())).
normalize_html_attrs(Proplist) ->
    wpart_utils:normalize_html_attrs(Proplist).

-spec(fdelete/1 :: (string()) -> ok).
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
