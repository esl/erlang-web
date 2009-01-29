#!/usr/bin/env escript

main(Args) ->
    inets:start(),
    action(Args).

action(["list"]) ->
    Result = make_request("list.py"),

    lists:foreach(fun(R) ->
			  write_result(R)
		  end, lists:sort(string:tokens(Result, "\n")));
action(["search", Keyword]) ->
    Result = make_request("search.py?string=" ++ Keyword),
    
    lists:foreach(fun(R) ->
			  write_result(R)
		  end, lists:sort(string:tokens(Result, "\n")));
action(["details", Name]) ->
    Result = make_request("search.py?string=" ++ Name),
    
    lists:foreach(fun(R) ->
			  write_detailed_result(R)
		  end, lists:sort(string:tokens(Result, "\n")));
action(["install", Name]) ->
    Result = make_request("download.py?name=" ++ Name),

    erl_tar:extract({binary, list_to_binary(Result)},
		    [{cwd, "../lib/"},
		     verbose,
		     compressed,
		     keep_old_files]),
    
    io:format("~s e_component installed successfully~n", [Name]);
action(_) ->
    io:format("Usage: ~n"
	      "e_component.erl list - lists all of the components stored in the repository~n"
	      "e_component.erl search Keyword - searches in the repository for the component having "
	      "Keyword in its name or description~n"
	      "e_component.erl details Keyword - prints out the detailed information about the given "
	      "component~n"
	      "e_component.erl install Name - downloads and installs the ecomponent into the existing "
	      "Erlang Web project~n").

write_result(Desc) ->
    [Name, Vsn, Description, _Categories, _Author] = string:tokens(Desc, "\t"),
    io:format("~s ~s~n", [string:left(Name ++ "-" ++ Vsn, 40, $ ), Description]).

write_detailed_result(Desc) ->
    [Name, Vsn, Description, Categories, Author] = string:tokens(Desc, "\t"),
    io:format("Name: ~s Version: ~s\tAuthor: ~s~n", [string:left(Name, 30, $ ), Vsn, Author]),
    io:format("Description:~n~s~n", [Description]),
    io:format("Categories: ~s~n~n", [Categories]).
    
make_request(Type) ->
    case http:request("http://ecomponents.erlang-web.org/cgi-bin/" ++ Type) of
	{ok, {_, _, Result}} ->
	    Result;
	{error, Reason} ->
	    io:format("Error during making the ~p request, reason: ~p~n",
		      [Type, Reason]),
	    []
    end.
