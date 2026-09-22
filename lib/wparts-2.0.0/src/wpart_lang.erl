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
-module(wpart_lang).
-export([handle_call/1, get_translation/1, get_lang/0]).
-export([get_lang_list/0]).

-include_lib("xmerl/include/xmerl.hrl").

%% @doc Generates the translation of the element with given key.
%% <p>First step to support i18n in our project is to edit <i>config/project.conf</i> file.
%% It should contain at least one config tuple:<br/>
%% <i>{language_files, [Files]}.</i><br/>
%% where Files = [{Lang, File}, Files] | _<br/> 
%% Lang = two-letter shortcut for language (e.g. en for English) and <br/>
%% File = path to the translation file.<br/>
%% Second step is to declare the default language by placing such a tuple inside the config file:<br/>
%% <i>{default_language, Lang}.</i><br/></p>
%% <p>Language discovery is performed in given order:<br/>
%% <ul><li>it is searched under session:lang key in e_dict</li>
%% <li>got from conf file (from the default language)</li>
%% <li>eventually English language is selected</li></ul></p>
%% <p>The translation file entries should be in the following format:<br/>
%% <i>{Key, Translation}.</i><br/>
%% Where key is either a single string, or a tuple of strings.
%% The difference is when no translation for given key is found, e_lang 
%% returns the key itself in case when key is a string, 
%% or "no_translation_found" otherwise.</p>
%% <p>Wpart should look like:<br/>
%% <i>&lt;wpart:lang key=Key/&gt;</i><br/>
%% If Key contains ":", it is split by it, and the result tokens creates
%% a key tuple (e.g. key="this:is:a:test" becomes a key tuple: <br/>
%% <i>{"this", "is", "a", "test"}</i>). Otherwise the string is the key.</p>
%% <p>If we want to place some translations in record definitions
%% (in description parameter), it's value should be in format:<br/>
%% <i>{key, Key}</i><br/>
%% where Key is the same key as in wpart:lang.</p>
handle_call(E) ->
    KeyAttr = wpartlib:attribute("attribute::key", E),

    #xmlText{value=get_translation(KeyAttr),
	     type=cdata}.

get_lang() ->
    case wpart:fget("session:lang") of
	undefined ->
	    e_conf:default_language(); 
	Val ->
	    list_to_atom(Val)
    end.

get_lang_list() ->
    case wpart:fget("session:lang") of
	undefined ->
	    atom_to_list(e_conf:default_language());
	Val ->
	    Val
    end.

get_translation(KeyAttr) ->
    Lang = get_lang(),

    Key = case string:chr(KeyAttr, $:) of
	      0 -> KeyAttr;
	      _ -> list_to_tuple(string:tokens(KeyAttr, ":"))
	  end,

    e_lang:translate(Lang, Key).
