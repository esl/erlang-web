-type(string_proplist() :: list({string(), string()})).

-record(request, {
	  url :: string(),
	  cookies = [] :: string_proplist(),
	  post_args = [] :: list({string(), string()}),
	  get_args = [] :: list({string(), string()}),
	  protocol = http :: http | https
%% TODO - add multipart?
	 }).

-record(response, {
	  headers :: list({string(), string()}),
	  code = 200 :: integer(),
	  cookies :: list({string(), string()}),
	  body = "" :: string(),
	  req_dict :: list()
	 }).

-undef(assertDbg).
-define(assertDbg(Expr, MinPassed),
	(fun() ->
		 case Expr of
		     {0, 0} ->
			 ewts_dbg:clear();
		     {N, 0} when MinPassed =< N ->
			 ewts_dbg:clear();
		     {N, M} ->
			 ewts_dbg:clear(),
			 erlang:error({assertDbg_failed,
				       [{module, ?MODULE},
					{line, ?LINE},
					{expression, (??Expr)},
					{dbg_tests_failed, (??M)},
					{dbg_tests_passed, (??N)}]})
		     end
	 end)()).
