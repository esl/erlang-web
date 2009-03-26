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
%%% File    : e_dispatcher.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%%
%%% @doc Module responsible for dispatching the incoming requests.
%%% <p>The rules for the proper routing are described inside the <i>dispatch.conf</i>
%%% file in the <i>config/</i> directory in the server root. 
%%% The second mandatory file is <i>errors.conf</i> which 
%%% describes, which pages should be displayed to the user in case
%%% of particular error.</p>
%%%
%%% <p>The dispatcher configuration files must contain the Erlang tuples
%%% (must be parsable by <i>file:consult</i>).<br/>
%%% The following format of the tuples are available:
%%% <ul>
%%% <li><i>{dynamic, Regexp, {Mod, Fun}}</i> - if the incoming URL matches the <i>Regexp</i>, 
%%% then the <i>Mod:Fun</i> is called to serve the request</li>
%%% <li><i>{dynamic, delegate, Regexp, Path}</i> - the meta-entry. During the start
%%% of the dispatcher, the file stored under the <i>Path</i> is read and all the entries
%%% inside it are prefixed by <i>Regexp</i>. Then, they are merged with the 
%%% previously loaded rules.</li>
%%% <li><i>{static, Regexp, Path}</i> - if the incoming URL matches the <i>Regexp</i>,
%%% then the static page (available under <i>Path</i>) will be expanded.</li>
%%% <li><i>{static, Regexp, enoent}</i> - if the incoming URL matches the <i>Regexp</i>
%%% the corresponding file from the docroot directory will be served directly
%%% by the server.</li>
%%% </ul></p>
%%% 
%%% <p>There is a possibility to retrive some values from the URLs and pass them to the 
%%% called function (it is only possible when the rule type is set to <i>dynamic</i>).<br/>
%%% In order to fetch the values, the regular expression must be in the specific format.
%%% The format is described on [http://www.erlang.org/doc/man/re.html], in named 
%%% subpatterns section.<br/>
%%% The obtained list of pairs: <i>{Name, Value}</i> (both strings) will be passed 
%%% as an argument to the first function on the dataflow list.
%%% </p>
%%% @end
%%%-------------------------------------------------------------------
-module(e_dispatcher).

-export([error_page/1]).
-export([install/0, reinstall/0]).
-export([is_static/1, dispatch/1]).
-export([fe_dispatch/1, add_rule/1]).

-type(dispatcher_result() :: {atom(), atom(), nil()} | {view, string()} | invalid_url | {error, 404, string()}).
-type(cache_type() :: no_cache | normal | persistent | {timeout, integer()}).

%%
%% @spec install() -> ok
%% @doc Loads the configuration files and creates the dispatcher ets tables.
%%
-spec(install/0 :: () -> ok).	     
install() ->
    Patterns = load(),
    {Static, Dynamic} = divide(Patterns),

    ets:new(?MODULE, [named_table, public]),
    ets:insert(?MODULE, [{dynamic, Dynamic}, {static, Static}]),
    
    load_errors().

%%
%% @spec reinstall() -> ok
%% @doc Reloads the configuration files and clears the old dispatcher ets tables.
%%
-spec(reinstall/0 :: () -> ok).	     
reinstall() ->
    ets:delete_all_objects(?MODULE),

    Patterns = load(),
    {Static, Dynamic} = divide(Patterns),

    ets:insert(?MODULE, [{dynamic, Dynamic}, {static, Static}]),
    load_errors().
%%
%% @spec is_static(URL :: string()) -> bool()
%% @doc Checks if the URL should be served directly from the docroot.
%%
-spec(is_static/1 :: (string()) -> bool()).	     
is_static(Path) ->
    [{static, Static}] = ets:lookup(?MODULE, static),
    case dispatch(Path, Static) of 
	invalid_url -> true;
	_ -> false
    end.

%%
%% @spec dispatch(URL :: string()) -> {Mod :: atom(), Fun :: atom(), []} |
%% {view, View :: string()} | invalid_url | {error, 404, URL :: string()}
%% @doc Makes a decision, based on loaded rules, how to serve the request.
%% If the request should be served dynamically, the <i>{Mod, Fun, []}</i>
%% is returned (the named subpattern captured elements are stored under
%% <i>"__dispatcher_params"</i> inside the request dictionary). <br/>
%% If the <i>{view, View}</i> is returned, then the rule was static and
%% the proper template should be expanded.<br/>
%% In other case, when <i>invalid_url</i> is returned, the content should be
%% served directly from docroot.<br/>
%% Otherwise, the error page with code 404 should be displayed.
%% @end
%%
-spec(dispatch/1 :: (string()) -> dispatcher_result()).
dispatch(Path) ->
    [{static, Static}] = ets:lookup(?MODULE, static),
    [{dynamic, Dynamic}] = ets:lookup(?MODULE, dynamic),

    dispatch(Path, lists:append([Dynamic, Static])).

-spec(fe_dispatch/1 :: (string()) -> {cache_type(), dispatcher_result()}).
fe_dispatch(Url) ->
    [{static, Static}] = ets:lookup(?MODULE, static),
    [{dynamic, Dynamic}] = ets:lookup(?MODULE, dynamic),

    Selector = fun(X) -> selector(Url, X) end,
    Action = case filter(Selector, lists:append([Dynamic, Static])) of
		 {ok, A} -> A;
		 nomatch -> nomatch
	     end,

    fe_process(Action, Url).

%%
%% @spec error_page(ErrorCode :: integer()) -> TemplatePath :: string() | not_found
%% @doc Returns path to the page that should be rendered in case of error with <i>ErrorCode</i>.
%% If the proper page for the given <i>ErrorCode</i> is not defined, 
%% <i>not_found</i> is returned.
%% @end
%%
-spec(error_page/1 :: (integer()) -> string() | not_found).	     
error_page(Nr) ->
    case ets:lookup(?MODULE, {error, Nr}) of
	[{{error, Nr}, Path}] ->
	    Path;
	[] ->
	    not_found
    end.

%% main handler: it selects the first match in patterns list
%% and properly processes it
-spec(dispatch/2 :: (string(), list(tuple())) -> dispatcher_result()).
dispatch(URL, Patterns) ->
    Selector = fun(X) -> selector(URL, X) end,
    Action = case filter(Selector, Patterns) of
		 {ok, A} -> A;
		 nomatch -> nomatch
	     end,
    process(Action, URL).
    
-spec(divide/1 :: (list(tuple())) -> {list(tuple()), list(tuple())}).	     
divide(List) ->
    divide(List, [], []).

-spec(divide/3 :: (list(tuple()), list(tuple()), list(tuple())) -> {list(tuple()), list(tuple())}).	   
divide([], Static, Dynamic) ->
    {lists:reverse(Static), lists:reverse(Dynamic)};
divide([{static, Regexp, Action, Opts} | Rest], Static, Dynamic) ->
    {ok, Compiled} = re:compile(Regexp),
    T = {static, Compiled, Action, Opts},
    divide(Rest, [T | Static], Dynamic);
divide([{dynamic, Regexp, Action, Opts} | Rest], Static, Dynamic) ->
    {ok, Compiled} = re:compile(Regexp),
    T = {dynamic, Compiled, Action, Opts},
    divide(Rest, Static, [T | Dynamic]);
divide([{alias, RegexpSrc, RegexpTrgt, Opts} | Rest], Static, Dynamic) ->
    {ok, Compiled} = re:compile(RegexpSrc),
    T = {alias, Compiled, RegexpTrgt, Opts},
    divide(Rest, Static, [T | Dynamic]).

-spec(parser/2 :: (tuple(), tuple()) -> tuple() | list(tuple())).	     
parser({include, Filename}, _) ->
    load(Filename);
parser({dynamic, delegate, Prefix, Filename}, Compiled) ->
    Adder = fun({dynamic, delegate, Prefix2, Filename2}) ->
		    parser({dynamic, delegate, Prefix ++ Prefix2, Filename2}, Compiled);
	       ({Type, Regexp, Arg, Opts}) when is_list(Regexp) ->
		    {Type, Prefix ++ Regexp, Arg, Opts};
	       ({Type, Regexp, Arg}) ->
		    {Type, Prefix ++ Regexp, Arg, []}
	    end,
    lists:map(Adder, load(Filename));
parser({dynamic, Regexp, ModFun, Options} = Org, Compiled) -> 
    case get_all_names(Regexp, Compiled) of
	[] -> 
	    Org;
	Names ->
	    {dynamic, Regexp, ModFun, [{named_subpatterns, Names} | Options]}
    end;
parser({alias, Regexp, RegexpTrgt, Opts} = Org, Compiled) ->
    case get_all_names(Regexp, Compiled) of
	[] ->
	    Org;
	ANames ->
	    SNames = lists:map(fun atom_to_list/1, ANames),
	    {alias, Regexp, RegexpTrgt, [{substitutions, SNames} | Opts]}
    end;
parser({Type, Opt1, Opt2}, Compiled) ->
    parser({Type, Opt1, Opt2, []}, Compiled);
parser(X, _) ->
    X.

-spec(load/0 :: () -> list(tuple())).	     
load() ->
    DispConf = filename:join([e_conf:server_root(), "config", "dispatch.conf"]),
    lists:flatten(load(DispConf)).

-spec(load/1 :: (string()) -> list(tuple())).
load(Filename) ->
    {ok, Terms} = file:consult(Filename),
    {ok, NamedRegexp} = re:compile("\\?<[A-Za-z_0-9]+>", [ungreedy]),

    lists:map(fun(X) -> parser(X, NamedRegexp) end, Terms).

%% Returns first element which satisfies Fun
-spec(filter/2 :: (fun(), list(tuple())) -> {ok, tuple()} | nomatch).	    
filter(_Fun, []) -> 
    nomatch;
filter(Fun, [First|Rest]) ->
    case Fun(First) of 
	true ->
	    {ok, First};
        false -> 
	    filter(Fun, Rest)
    end.

-spec(selector/2 :: (string(), tuple()) -> bool()).	     
selector(Element, {_, Regexp, _, []}) -> 
    selector_exec(Element, Regexp);
selector(Element, {_, Regexp, _, Opts}) ->
    selector_exec(Element, Regexp, Opts).

-spec(selector_exec/2 :: (string(), tuple()) -> bool()).	     
selector_exec(Element, Regexp) ->
     case re:run(Element, Regexp, [{capture,first}]) of
	{match, _} ->
	    true;
	nomatch ->
	    false;
	{error, Reason} ->
	    exit({?MODULE, Reason})
    end.

-spec(selector_exec/3 :: (string(), tuple(), list(tuple())) -> bool()).	     
selector_exec(Element, Regexp, Opts) ->
    case lists:keysearch(named_subpatterns, 1, Opts) of
	false ->
	    selector_exec(Element, Regexp);
	{_, {_, Names}} ->
	    case re:run(Element, Regexp, [{capture, Names, list}]) of
		nomatch ->
		    false;
		match ->
		    true;
		{match, Matched} ->
		    Zipped = lists:zip(Names, Matched),
		    eptic:fset("__dispatcher_params", Zipped),
		    
		    true
	    end
    end.

-spec(process/2 :: (tuple(), string()) -> dispatcher_result()).	     
process({static, _, enoent, _}, _URL) ->
    invalid_url;
process({static, _, Path, _}, _) ->
    {view, Path};
process({dynamic, _, {Module, Function}, _Opts}, _URL) ->
    {Module, Function, []};
process({alias, Regexp, Target, Opts}, URL) ->
    dispatch(substitute_alias(URL, Regexp, Target, proplists:get_value(substitutions, Opts, [])));
process(nomatch, URL) ->
    {error, 404, URL}.

-spec(fe_process/2 :: (tuple(), string()) -> {cache_type(), dispatcher_result()}).	     
fe_process({static, _, enoent, _}, _) ->
    {no_cache, [], invalid_url};
fe_process({static, _, Path, Opts}, _) ->
    {proplists:get_value(cache, Opts, persistent), proplists:get_value(cache_groups, Opts, ["view"]), {view, Path}};
fe_process({dynamic, _, {M, F}, Opts}, _) ->
    {proplists:get_value(cache, Opts, normal), proplists:get_value(cache_groups, Opts, ["controller"]), {M, F, []}};
fe_process({alias, Regexp, Target, Opts}, URL) ->
    fe_dispatch(substitute_alias(URL, Regexp, Target, proplists:get_value(substitutions, Opts, [])));
fe_process(nomatch, URL) ->
    {persistent, ["errors"], {error, 404, URL}}.

-spec(load_errors/0 :: () -> ok).	     
load_errors() ->
    ErrConf = filename:join([e_conf:server_root(), "config", "errors.conf"]),
    {ok, Tuples} = file:consult(ErrConf),
    Inserter = fun({error, Nr, Tpl}) ->
		       ets:insert(?MODULE, {{error, Nr}, Tpl})
	       end,
    lists:foreach(Inserter, Tuples).

-spec(substitute_alias/4 :: (string(), tuple(), string(), list(atom())) -> string()).	     
substitute_alias(URL, Regexp, Target0, Substitutions) ->
    case re:run(URL, Regexp, [{capture, Substitutions, list}]) of
	match ->
	    Target0;
	{match, Matched} ->
	    lists:foldl(fun({Name, Replacement}, Target) ->
				re:replace(Target, "\\(\\?<" ++ Name ++ ">\\)", Replacement, [{return, list}])
			end, Target0, lists:zip(Substitutions, Matched))
    end.

-spec(get_all_names/2 :: (string(), tuple()) -> list(atom())).	     
get_all_names(String, Regexp) ->
    get_all_names(String, Regexp, []).

-spec(get_all_names/3 :: (string(), tuple(), list(atom())) -> list(atom())).	     
get_all_names(String, Regexp, Names) ->
    case re:run(String, Regexp, [{capture, all}]) of
	nomatch ->
	    Names;
	{match, [{Start, Len}]} ->
	    Name = list_to_atom(string:substr(String, Start+3, Len-3)),
	    get_all_names(string:substr(String, Start+Len), Regexp, [Name | Names])
    end.

-spec(add_rule/1 :: (tuple()) -> true).	     
add_rule({Type, Regexp, Rule}) ->
    add_rule(Type, Regexp, Rule, []);
add_rule({Type, Regexp, Rule, Opts}) ->
    add_rule(Type, Regexp, Rule, Opts).
    
-spec(add_rule/4 :: (static | dynamic, string(), term(), list()) -> true).	     
add_rule(Type, Regexp, Target, Opts) ->
    [{_, TypeRules}] = ets:lookup(?MODULE, Type),
    {ok, Compiled} = re:compile(Regexp),
    
    ets:insert(?MODULE, {Type, lists:append([{Type, Compiled, Target, Opts}], TypeRules)}).
