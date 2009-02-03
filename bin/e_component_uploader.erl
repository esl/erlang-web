#!/usr/bin/env escript

main(Args) ->
    inets:start(),
    application:start(crypto),
    action(Args).

action(["add", Path]) ->
    [AppName] = filelib:wildcard(Path ++ "/ebin/*.app"),
    {ok, [{application, Name0, Props}]} = file:consult(AppName),
    
    Name = atom_to_list(Name0),
    Content = read_package(create_package(Name, Path)),

    Username = string:strip(io:get_line("Username: "), both, 10),
    Password = binary_to_list(crypto:md5(string:strip(io:get_line("Password: "), both, 10))),

    Categories = string:strip(io:get_line("Enter the categories for the ecomponent (separated by \",\"): "), both, 10),
    Vsn = proplists:get_value(vsn, Props),
    Desc = proplists:get_value(description, Props),
    Author = proplists:get_value(author, Props, Username),

    Params = string:join(lists:map(fun({Key, Val}) ->
					   Key ++ "=" ++ edoc_lib:escape_uri(Val)
				   end, [{"username", Username},
					 {"password", Password},
					 {"name", Name},
					 {"version", Vsn},
					 {"categories", Categories},
					 {"description", Desc},
					 {"author", Author},
					 {"package", Content}]), [$&]),

    {ok, {_, _, Response}} = http:request(post, {"http://ecomponents.erlang-web.org/cgi-bin/add.py", 
						 [{"content-length", integer_to_list(length(Params))},
						  {"user-agent", escript:script_name()}], 
						 "application/x-www-form-urlencoded", Params}, 
					  [], []),
    io:format("~s~n", [Response]).

create_package(Name, Path) ->
    TarName = "/tmp/" ++ Name ++ ".tar.gz",
    erl_tar:create(TarName, [Path], [compressed]),
    TarName.
    
read_package(Path) ->
    {ok, Binary} = file:read_file(Path),
    file:delete(Path),
    binary_to_list(Binary).

hash(String) ->
    lists:flatten(lists:reverse(lists:foldl(fun(X, Acc) ->
						    int_to_hex(X, Acc)
					    end, [], String))).

int_to_hex(X, Acc) when X < 256 ->
    [[hex(X div 16), hex(X rem 16)] | Acc].

hex(X) when X < 10 ->
    $0 + X;
hex(X) ->
    $a + (X-10).
