#!/usr/bin/env escript

main(["list"]) ->
    inets:start(),
    Result = make_request("list.py"),

    lists:foreach(fun(R) ->
			  write_result(R)
		  end, lists:sort(string:tokens(Result, "\n"))).

write_result(Desc) ->
    [Name, Vsn, Description, _Categories, _Author] = string:tokens(Desc, "\t"),
    io:format("~s-~s\t~s~n", [Name, Vsn, Description]).
    
make_request(Type) ->
    case http:request("http://ecomponents.erlang-web.org/cgi-bin/" ++ Type) of
	{ok, {_, _, Result}} ->
	    Result;
	{error, Reason} ->
	    io:format("Error during making the ~p request, reason: ~p~n",
		      [Type, Reason]),
	    []
    end.
