%%%-------------------------------------------------------------------
%%% File    : ewts_client.erl
%%% Author  : michalptaszek <michalptaszek@paulgray>
%%% Description : 
%%%
%%% Created : 18 Jun 2009 by michalptaszek <michalptaszek@paulgray>
%%%-------------------------------------------------------------------
-module(ewts_client).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([request/1, request/2, request/3, request/4, request/5, do_request/1]).
-export([direct_request/1, direct_request/2, direct_request/3, 
	 direct_request/4, direct_request/5, do_direct_request/1]).
-export([clear/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("eptic/include/eptic.hrl").
-include_lib("ewts/include/request.hrl").

-record(state, {eptic_cookie = ""}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
-spec(start_link/0 :: () -> any()).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec(request/1 :: (string()) -> tuple()).
request(Url) ->
    do_request(#request{url = Url}).

-spec(request/2 :: (string(), string_proplist()) -> tuple()).
request(Url, Get) ->
    do_request(#request{url = Url, get_args = Get}).

-spec(request/3 :: (string(), string_proplist(), string_proplist()) ->
	     tuple()).
request(Url, Get, Post) ->
    do_request(#request{url = Url, get_args = Get, post_args = Post}).

-spec(request/4 :: (string(), string_proplist(), string_proplist(),
		    string_proplist()) -> tuple()).
request(Url, Get, Post, Cookies) ->
    do_request(#request{url = Url, get_args = Get, post_args = Post, 
			cookies = Cookies}).

-spec(request/5 :: (string(), string_proplist(), string_proplist(),
		    string_proplist(), http | https) -> tuple()).
request(Url, Get, Post, Cookies, Protocol) ->
    do_request(#request{url = Url, get_args = Get, post_args = Post,
			cookies = Cookies, protocol = Protocol}).

-spec(direct_request/1 :: (string()) -> tuple()).
direct_request(Url) ->
    do_direct_request(#request{url = Url}).

-spec(direct_request/2 :: (string(), string_proplist()) -> tuple()).
direct_request(Url, Get) ->
    do_direct_request(#request{url = Url, get_args = Get}).

-spec(direct_request/3 :: (string(), string_proplist(), string_proplist()) ->
	     tuple()).
direct_request(Url, Get, Post) ->
    do_direct_request(#request{url = Url, get_args = Get, post_args = Post}).

-spec(direct_request/4 :: (string(), string_proplist(), string_proplist(),
		    string_proplist()) -> tuple()).
direct_request(Url, Get, Post, Cookies) ->
    do_direct_request(#request{url = Url, get_args = Get, post_args = Post, 
			cookies = Cookies}).

-spec(direct_request/5 :: (string(), string_proplist(), string_proplist(),
		    string_proplist(), http | https) -> tuple()).
direct_request(Url, Get, Post, Cookies, Protocol) ->
    do_direct_request(#request{url = Url, get_args = Get, post_args = Post,
			cookies = Cookies, protocol = Protocol}).

-spec(do_request/1 :: (tuple()) -> tuple()).
do_request(Req) ->
    Response = gen_server:call(?MODULE, {request, Req}),
    Response#response{body = normalize_body(Response#response.body)}.

-spec(do_direct_request/1 :: (tuple()) -> tuple()).
do_direct_request(Req) ->
    Response = e_mod_ewts:handle_request(Req),
    Response#response{body = normalize_body(Response#response.body)}.

-spec(clear/0 :: () -> ok).
clear() ->
    gen_server:cast(?MODULE, clear).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
-spec(init/1 :: (nil()) -> {ok, tuple()}).
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
-spec(handle_call/3 :: (term(), term(), tuple()) -> tuple()).
handle_call({request, Req}, _From, State) ->
    Request = case Req#request.cookies of
		  [] ->
		      Req#request{cookies = State#state.eptic_cookie};
		  _ ->
		      Req
	      end,
    Response = e_mod_ewts:handle_request(Request),

    {reply, Response, 
     State#state{eptic_cookie = get_eptic_cookie(Response)}}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
-spec(handle_cast/2 :: (term(), tuple()) -> tuple()).
handle_cast(clear, State) ->
    {noreply, State#state{eptic_cookie = []}}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
-spec(handle_info/2 :: (term(), tuple()) -> tuple()).
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
-spec(terminate/2 :: (term(), tuple()) -> ok).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
-spec(code_change/3 :: (term(), tuple(), term()) -> tuple()).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec(get_eptic_cookie/1 :: (tuple()) -> none | tuple()).
get_eptic_cookie(#response{cookies = Cookies}) ->
    case proplists:lookup(?COOKIE, Cookies) of
	none ->
	    [];
	Val ->
	    [Val]
    end.

-spec(normalize_body/1 :: (list()) -> string()).
normalize_body(Body) ->
    lists:flatten(lists:map(fun(Part) when is_binary(Part) ->
				    binary_to_list(Part);
			       (Else) ->
				    Else
			    end, lists:flatten(Body))).
