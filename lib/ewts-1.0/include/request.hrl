-type(string_proplist() :: list({string(), string()})).

-record(request, {
	  url :: string(),
	  cookies = [] :: string_proplist(),
	  post_args = [] :: string_proplist(),
	  get_args = [] :: string_proplist(),
	  protocol = http :: http | https
%% TODO - add multipart?
	 }).

-record(response, {
	  headers :: string_proplist(),
	  code = 200 :: integer(),
	  cookies :: string_proplist(),
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
