%%%-------------------------------------------------------------------
%%% File    : ewts_dbg.erl
%%% Author  : michalptaszek <michalptaszek@paulgray>
%%% Description : DBG callback module for EWTS
%%%
%%% Created : 29 Jun 2009 by michalptaszek <michalptaszek@paulgray>
%%%-------------------------------------------------------------------
-module(ewts_dbg).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([add_case/1, results/0, clear/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {events = [],
		passed = 0,
		failed = 0}).

-include_lib("stdlib/include/ms_transform.hrl").

-define(TEST_FAILED(Test, Exp, Got), 
	io:format("EWTS ~w test has failed. Expected ~p, got ~p~n~n",
		  [Test, Exp, Got])).

-spec(start_link/0 :: () -> any()).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec(results/0 :: () -> {integer(), integer()}).
results() ->
    gen_server:call(?MODULE, results).

-spec(clear/0 :: () -> any()).
clear() ->
    dbg:ctpl(),
    dbg:tp(?MODULE, clear, 0, []),
    dbg:tpl(?MODULE, add_to_registered_funs, 3, []),
    gen_server:cast(?MODULE, clear).

-spec(add_case/1 :: (tuple()) -> ok).
add_case({dispatcher_result, _} = Case) ->
    dbg:tp(e_dispatcher, dispatch, 1, 
	   dbg:fun2ms(fun(_) -> return_trace() end)),
    
    gen_server:cast(?MODULE, {add_case, Case});
add_case({function_response, {M, F, A}, Res}) ->
    dbg:tp(M, F, A, 
	   dbg:fun2ms(fun(_) -> return_trace() end)),

    gen_server:cast(?MODULE, {add_case, {{function_response, {M, F, A}}, Res}}),
    add_to_registered_funs(M, F, A).
    

-spec(dbg_handler/2 :: (tuple(), term()) -> term()).
dbg_handler({trace, _, return_from, {e_dispatcher, dispatch, 1}, Result}, Data) ->
    gen_server:cast(?MODULE, {check_case, {dispatcher_result, Result}}),
    Data;
dbg_handler({trace, _, call, {?MODULE, clear, []}}, _) ->
    [];
dbg_handler({trace, _, call, {?MODULE, add_to_registered_funs, [M, F, A]}}, Data) ->
    [{M, F, A} | Data];
dbg_handler({trace, _, return_from, {M, F, A}, Result}, Data) ->
    case lists:member({M, F, A}, Data) of
	true ->
	    gen_server:cast(?MODULE, 
			    {check_case, {{function_response, {M, F, A}}, Result}});
	false ->
	    ok
    end,
    Data;
dbg_handler(_Event, Data) ->
    Data.

add_to_registered_funs(_, _, _) ->
    ok.

-spec(init/1 :: (nil()) -> {ok, tuple()}).
init([]) ->
    HandlerSpec = {fun dbg_handler/2, []},
    dbg:tracer(process, HandlerSpec),
    dbg:p(all, call),
    dbg:ctpl(),
    dbg:tp(?MODULE, clear, 0, []),
    dbg:tpl(?MODULE, add_to_registered_funs, 3, []),
    
    {ok, #state{}}.

-spec(handle_call/3 :: (term(), term(), tuple()) -> tuple()).
handle_call(results, _From, State) ->
    Reply = {State#state.passed, State#state.failed},

    {reply, Reply, State}.

-spec(handle_cast/2 :: (term(), tuple()) -> tuple()).
handle_cast(clear, _State) ->
    {noreply, #state{}};

handle_cast({add_case, Case}, State) ->
    {noreply, State#state{events = [Case | State#state.events]}};

handle_cast({check_case, {Key, Val}}, State) ->
    case proplists:get_value(Key, State#state.events, Val) of
	Val ->
	    {noreply, inc_passed(State)};
	Other ->
	    ?TEST_FAILED(Key, Other, Val),
	    {noreply, inc_failed(State)}
    end.


-spec(handle_info/2 :: (term(), tuple()) -> tuple()).
handle_info(_Info, State) ->
    {noreply, State}.

-spec(terminate/2 :: (term(), tuple()) -> ok).
terminate(_Reason, _State) ->
    ok.

-spec(code_change/3 :: (term(), tuple(), term()) -> tuple()).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec(inc_passed/1 :: (tuple()) -> tuple()).
inc_passed(#state{passed = N} = State) ->
    State#state{passed = N+1}.

-spec(inc_failed/1 :: (tuple()) -> tuple()).
inc_failed(#state{failed = N} = State) ->
    State#state{failed = N+1}.
