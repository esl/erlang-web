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
%%% File    : e_lang.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc Module responsible for translating the contents of the page.
%%% <p>The translations are read form the translation files (usually placed
%%% in <i>config/languages/</i> directory (they should be specified in
%%% <i>project.conf</i> under <i>language_files</i> key).
%%% The language_files configuration tuple should look like: 
%%% <i>{langauage_files, {Language, PathToTranslationFile}}</i>.<br/>
%%% The translation files should contain a two element tuples: 
%%% <i>{Key, Translation}</i> where <i>Key</i> should be string or a 
%%% tuple of strings.</p>
%%% @end
%%%-------------------------------------------------------------------

-module(e_lang).
-export([install/0, reinstall/0]).
-export([list/0, translate/2]).

%%
%% @spec install() -> none()
%% @doc Loads the translation files and creates the proper ets table.
%%
-spec(install/0 :: () -> none()).	     
install() ->
    Languages = load(),
    
    ets:new(e_languages, [named_table, public]),
    ets:insert(e_languages, Languages).

%%
%% @spec reinstall() -> none()
%% @doc Clears the translation and reloads the translation files.
%% @todo in case of e_ets_cache acceptance, flush all the cached files
%%
-spec(reinstall/0 :: () -> none()).	     
reinstall() ->
    ets:delete_all_objects(e_languages),

    Languages = load(),
    ets:insert(e_languages, Languages).

%% 
%% @spec translate(Language :: atom(), Key :: string() | tuple()) -> string()
%% @doc Translates the given <i>Key</i>.
%% If no translation for the given <i>Language</i> is provided and the <i>Key</i>
%% is a tuple, then the <i>Key</i> is returned. 
%% Otherwise, "no translation found" sign is returned.
%%
-spec(translate/2 :: (atom(), string() | tuple()) -> string()).	     
translate(Language, Key) ->
    case ets:lookup(e_languages, {Language, Key}) of
	[{_, X}] -> X;
	[] when is_list(Key) -> Key;
	_ -> "no translation found"
    end.

%%
%% @spec list() -> list()
%% @doc Returns the list of all possible translations.
%% The list is in format: <i>{{Language, Key}, Translation}</i>.
%% 
-spec(list/0 :: () -> list(tuple())).	     
list() ->
    ets:tab2list(e_languages).

%%
%% @spec load() -> list(tuple())
%% @doc Loads the translation files.
%% If no language files are provided, the default language is taken
%% and the path to the translation file is set to 
%% <i>config/languages/DefaulLanguageCode.conf</i>.
%% @see e_conf:default_language/0
%%
-spec(load/0 :: () -> list(tuple())).	     
load() ->
    case ets:lookup(e_conf, language_files) of
	[] ->
	    Default = e_conf:default_language(),
	    EnConf = filename:join([e_conf:server_root(), 
				    "config",
                                    "languages", 
				    atom_to_list(Default) ++ ".conf"]),
	    load([{Default, EnConf}]);
	[{_, FileList}] ->
	    load(FileList)
    end.

%% Translation file should be in format:
%% {Key, Translation}.
%% where Key is an arbitary erlang term (string/atom/list/tuple)
%% and Translation is string which will be shown to user.
-spec(load/1 :: (list({atom(), string()})) -> list(tuple())).		      
load([]) ->
    [];
load([{Lang, Filename} | Rest]) ->
    case file:consult(Filename) of
	{ok, Translations} ->
	    lists:append(lists:map(fun({Key, Translation}) ->
					   {{Lang, Key}, Translation}
				   end, Translations),
			 load(Rest));
	{error, Reason} ->
	    error_logger:info_msg("~p module: error during loading file: ~p, reason:~p~n", 
				  [?MODULE, Filename, Reason]),
	    load(Rest)
    end.
