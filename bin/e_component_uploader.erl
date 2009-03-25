#!/usr/bin/env escript

main(Args) ->
    inets:start(),
    application:start(crypto),
    action(Args).

action(["add", Path]) ->
    [AppName] = filelib:wildcard(Path ++ "/ebin/*.app"),
    {ok, [{application, Name0, Props}]} = file:consult(AppName),
    Md5Ctx = crypto:md5_init(),
    
    Name = atom_to_list(Name0),
    Content = read_package(create_package(Name, Path)),

    Username = string:strip(io:get_line("Username: "), both, 10),
    ContentCtx = crypto:md5_update(Md5Ctx, Content),
    PasswdCtx = crypto:md5_update(ContentCtx, string:strip(io:get_line("Password: "), both, 10)),
    FinalMd5 = hash(binary_to_list(crypto:md5_final(PasswdCtx))),

    Categories = string:strip(io:get_line("Enter the categories for the ecomponent (separated by \",\"): "), both, 10),
    Vsn = proplists:get_value(vsn, Props),
    Desc = proplists:get_value(description, Props),
    Author = proplists:get_value(author, Props, Username),

    Params = string:join(lists:map(fun({Key, Val}) ->
					   Key ++ "=" ++ edoc_lib:escape_uri(Val)
				   end, [{"username", Username},
					 {"md5", FinalMd5},
					 {"name", Name},
					 {"version", Vsn},
					 {"categories", Categories},
					 {"description", Desc},
					 {"author", Author},
					 {"package", base64:encode_to_string(Content)}]), [$&]),

    {ok, {_, _, Response}} = http:request(post, {"http://ecomponents.erlang-web.org/cgi-bin/add.py", 
						 [{"content-length", integer_to_list(length(Params))},
						  {"user-agent", escript:script_name()}], 
						 "application/x-www-form-urlencoded", Params}, 
					  [], []),
    io:format("~s~n", [Response]);
action(_) ->
    print_usage().

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

print_usage() ->
    io:format("Usage: bin/e_component_uploader.erl add Path~n"
	      "where Path specifies the path to the valid Erlang application~n"
	      "adds the component to the e_component repository~n").
