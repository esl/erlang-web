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
%%% File	: e_mod_gen.erl
%%% @author Michal Ptaszek <michal.ptaszek@erlang-consulting.com>
%%% @doc Generic mod for the web servers.
%%% It deals with all generic parts of requests handling.
%%% @type controller_response() = template | {redirect, URL :: string} |
%%%   {content, html, HTML :: string()} | {content, text, Text :: string()} |
%%%   {json, Term :: term()} | {template, Template :: string()} |
%%%   {custom, Custom :: term()} | {headers, Headers :: list(tuple()), Res :: controller_response()} |
%%%   {error, Code :: integer()}
%%% @see e_mod_inets
%%% @see e_mod_yaws
%%% @end
%%%-------------------------------------------------------------------
-module(e_mod_gen).

-export([handle_request/1]).
-export([template/3, template_file/1, error_page/2, error_page/3]).
-export([restore_session/1, bind_session/1]).
-export([controller/3]).
-export([sanitize_file_name/1, parse_url/1]).

-include_lib("eptic/include/eptic.hrl").

-type(controller_response() :: template | 
      {redirect, string()} |
      {content, html, iolist()} | 
      {content, text, iolist()} | 
      {content, xml, iolist()} |
      {content, pdf, iolist()} |
      {json, term()} |
      {template, string()} | 
      {custom, term()} |
      %should be controller_response(), but recursive types are not
      %supported.
      {headers, list(tuple()), ControllerResponse::any()} |
      {error, integer()}).

%%
%% @spec handle_request(URL :: string()) -> HTML | list({status, Code :: integer()}, HTML) | 
%%   {ret_view, Ret :: controller_response(), View :: string()} | 
%%   enoent
%% HTML = {html, Html :: string()}
%% @doc Dispatches the request and calls the right module/expands the template.
%% The returning value is interpreted by the server callback module.
%%
-spec(handle_request/1 :: (string()) -> {html, string()} | 
					    list(tuple()) | 
					    {ret_view, controller_response(), string()} | 
					    enoent).
handle_request("/app/" ++ URL) ->
    e_dict:fset("__path", URL),
    e_logger:log({?MODULE, {old_path_type, URL}}),
    case parse_url(URL) of
	{view, View} -> view(View);
	{error, R} -> error_page(501, URL, R);
	{Mod, Fun, View} -> controller(Mod, Fun, View)
    end;
handle_request(URL) ->
    case e_dispatcher:dispatch(URL) of
	invalid_url -> enoent;
	{view, View} -> view(View);
	{error, Code, Path} -> error_page(Code, Path);
	{Mod, Fun, View} -> controller(Mod, Fun, View)
    end.

%%
%% @spec template(Path :: string(), [], Transform :: atom()) -> 
%%   list({status, ErrorCode}, {html, HTML :: string()}) | {html, HTML :: string()}
%% @doc Expands the template from the given <i>Path</i>.
%% The requested template is read from disk cache and properly expanded
%% using <i>Transform</i>:process_xml function (by default {@link //wpart/wpart_xs:process_xml/1}).
%% @see //wpart/wpart_xs:process_xml/1
%% @see e_cache:read_file/1
%%
-spec(template/3 :: (string(), nil(), atom()) -> list(tuple()) | {html, string()}).	     
template(Template, [], Transform) ->
    case e_cache:read_file(Template) of
	{error, Error} ->
	    error_page(404, Template, {e_cache_error, Error});
	E ->
	    {html, apply(Transform, process_xml, [E])}
    end.

%% 
%% @spec error_page(ErrorCode :: integer(), URL :: string()) -> 
%%   list({status, ErrorCode}, {html, HTML :: string()})
%% @doc Generates the error site for given error code.
%% If error code is 404, then calls the {@link error_page/3} with 
%% reason <i>not_found</i>.
%% In other cases, the reason is empty.
%% @see error_page/3
%% @see e_dispatcher:error_page/1
%%
-spec(error_page/2 :: (integer(), string()) -> list(tuple())).	     
error_page(404, Path) ->
    error_page(404, Path, not_found);
error_page(ErrorCode, Path) ->
    error_page(ErrorCode, Path, "").

%%
%% @spec error_page(ErrorCode :: integer(), URL ::string(), Reason :: term()) ->
%%   list({status, ErrorCode}, {html, HTML :: string()})
%% @doc Generates the error site with the reason <i>Reason</i> for given error code.
%% If the <i>debug_mode</i> flag is set to true, then the error page will always contain
%% the detailed information about the error.
%% Otherwise, the proper error page will be returned.
%% @see e_dispatcher:error_page/1
%% @see e_conf:debug_mode/0
%%
-spec(error_page/3 :: (integer(), string(), term()) -> list(tuple())).	     
error_page(ErrorCode, Path, Reason) ->
    error_logger:error_msg("~p module, error_page, error code: ~p~n"
			   "path: ~p~n"
			   "reason: ~p ~n", [?MODULE, ErrorCode, Path, Reason]),
    case e_dispatcher:error_page(ErrorCode) of
	not_found ->
	    [{status, ErrorCode},
	     {html, ?ERR(io_lib:print({not_found, Path}))}];
	TplPath ->
	    case e_conf:debug_mode() of
		true ->
		    [{status, ErrorCode},
		     {html, ?ERR(io_lib:print({Reason, Path}))}];
		false ->
                    Filled = 
                        apply(e_conf:template_expander(),
                              process_xml,
                              [e_cache:read_file(TplPath)]),

		    error_logger:error_msg("~p module, error: ~p: ~p~n",
                                           [?MODULE, ErrorCode, Path]),

		    [{status, ErrorCode},
		     {html, Filled}]
	    end
    end.

%%
%% @spec bind_session(Cookie :: term()) -> no_session | Cookie
%% @doc Binds the session to the given <i>Cookie</i>.
%% If the session kept in request dictionary is empty, then it deletes it 
%% from internal state and returns <i>no_session</i>.
%% Otherwise returns the given cookie.
%%   
-spec(bind_session/1 :: (Cookie) -> no_session | Cookie).	     
bind_session(Cookie) ->
    case {e_session:get_session(Cookie), e_dict:fget("session")} of
	{{ok,""},[]} ->
	    no_session;
	{_,[]} ->
	    e_session:delete_session(Cookie),
	    no_session;
	{_,Session} ->
	    e_session:update_session(Cookie,Session),
	    Cookie
    end.

%%
%% @spec restore_session(Cookie :: term()) -> true
%% @doc Restores the session with the given <i>Cookie</i>.
%% If the <i>Cookie</i> is empty, it sets the request dictionary 
%% session variable to empty list. 
%% Otherwise, it loads the previous state of session from
%% the internal data storage.
%%
-spec(restore_session/1 :: (term()) -> true).	     
restore_session("") ->
    e_dict:fset("session", []);
restore_session(Cookie) ->
    {ok, Session} = e_session:get_session(Cookie),
    e_dict:fset("session", Session).

%%
%% @spec template_file(Path :: string()) -> FullPath :: string()
%% @doc Returns the full path to the requested template.
%% The returned path is sanitized and prefixed with the template root
%% directory.
%% @see e_conf:template_root/0
%%
-spec(template_file/1 :: (string()) -> string()).	     
template_file(View) ->
    filename:join([
		   e_conf:template_root(),
		   sanitize_file_name(View)
		  ]).

%% @hidden
-spec(parse_url/1 :: (string()) -> {view, string()} | {atom(), atom(), string()} | {error, invalid_url}).
parse_url(Url) ->
    case string:tokens(Url, "/") of
	["view"|View] ->
            {view, filename:join(View)};
        [Mod, Fun | View] when length(View) /= 0 ->
            {list_to_existing_atom(Mod), 
             list_to_existing_atom(Fun),
             filename:join(View)};	  
        [Mod, Fun] ->
            {list_to_existing_atom(Mod), 
             list_to_existing_atom(Fun),
             []};	  
        _ ->
	    {error, invalid_url}
    end.

-spec(view/1 :: (string()) -> list(tuple()) | {html, string()}).	   
view(View) ->
    template(template_file(View), [], 
	     e_conf:template_expander()).

-spec(controller/3 :: (atom(), atom(), string()) -> {ret_view, controller_response(), string()}).							
controller(Mod, Fun, View) ->
    eptic:fset("__controller", Mod),
    Funs = Mod:module_info(exports),
    case (lists:member({dataflow,1},Funs) andalso lists:member({error,2},Funs)) of
	true ->
	    e_logger:log({?MODULE, {entering_dataflow_for, {Mod, Fun}}}),
	    Answ = apply(Mod, dataflow, [Fun]),
	    controller_handler(Answ, {Mod,Fun,View});
	false ->
	    case lists:member({validate, 1}, Funs) of
		true ->
		    e_logger:log({?MODULE, {skipping_dataflow, entering_validate, {Mod, Fun}}}),
		    {ok, ValidArgs} = apply(Mod, validate, [Fun]),
		    Ret = apply(Mod, Fun, ValidArgs),
		    e_logger:log({?MODULE, {controller_response, Ret}}),

		    {ret_view, Ret, View};
		false ->
		    e_logger:log({?MODULE, {skipping_dataflow_and_validate, entering_directly, {Mod, Fun}}}),
		    Ret = apply(Mod, Fun, [get_dataflow_initial_args()]),
		    e_logger:log({?MODULE, {controller_response, Ret}}),
		    
		    {ret_view, Ret, View}
	    end
    end.

-spec(controller_handler/2 :: ({list(atom()), list(atom())} | list(atom()), {atom(), atom(), string()}) ->
	     {ret_view, controller_response(), string()}).
controller_handler({Before, After}, {Mod,Fun,View}) ->
    e_logger:log({?MODULE, {dataflow_before, Before}}),
    e_logger:log({?MODULE, {dataflow_after, After}}),
    InitialArgs = get_dataflow_initial_args(),
    e_logger:log({?MODULE, {dataflow_dispatcher_args, InitialArgs}}),
    Ret = case dataflow(Mod, Fun, Before, InitialArgs) of
	      {ok, Args} ->
		  RetVal = apply(Mod,Fun,[Args]),
		  dataflow(Mod,Fun,After,[]),
		  RetVal;
	      {error, Val} -> 
		  Val
	  end,	  
    e_logger:log({?MODULE, {controller_response, Ret}}),
    {ret_view, Ret, View};
controller_handler(Before, {Mod,Fun,View}) ->
    e_logger:log({?MODULE, {dataflow_before, Before}}),
    InitialArgs = get_dataflow_initial_args(),
    e_logger:log({?MODULE, {dataflow_dispatcher_args, InitialArgs}}),
    Ret = case dataflow(Mod, Fun, Before, InitialArgs) of
	      {ok, Args} ->
		  apply(Mod,Fun,[Args]);
	      {error, Val} -> 
		  Val
	  end,	 
    e_logger:log({?MODULE, {controller_response, Ret}}),
    {ret_view, Ret, View}.

-spec(get_dataflow_initial_args/0 :: () -> list(tuple())).	     
get_dataflow_initial_args() ->
    case eptic:fget("__dispatcher_params") of
	undefined ->
	    [];
	Val ->
	    Val
    end.

-spec(dataflow/4 :: (atom(), atom(), list(atom()), term()) -> {ok, term()} | {error, controller_response()}).   
dataflow(_Mod, _Fun, [], Args) ->
    {ok, Args};
dataflow(Mod, Fun, [H|T], Args) ->
    case apply(Mod, H, [Fun, Args]) of
	{ok, Args1} ->
	    dataflow(Mod, Fun, T, Args1);
	{error, Reason} ->
	    {error, apply(Mod, error, [Fun, Reason])}
    end.

%% @hidden
-spec(sanitize_file_name/1 :: (string()) -> string()).	     
sanitize_file_name([$.,$.|T]) ->
    sanitize_file_name([$.|T]);
sanitize_file_name([H|T]) ->
    case lists:member(H, " &;'`{}!\\?<>\"()$") of
        true ->
            sanitize_file_name(T);
        false ->
            [H|sanitize_file_name(T)]
    end;
sanitize_file_name([]) ->
    [].
