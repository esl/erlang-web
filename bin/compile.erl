#!/usr/bin/env escript

main([]) ->
    make_basic();
main(["release", Vsn]) ->
    make_release(Vsn);
main(["rel", Vsn]) ->
    make_release(Vsn);
main(["release"]) ->
    make_release("0.1");
main(["rel"]) ->
    make_release("0.1");
main(["clean"]) ->
    clean();
main(_) ->
    usage().

make_basic() ->
    {ok, Dir} = file:list_dir("lib/"),
    [code:add_path("lib/" ++ D ++ "/ebin") || D <- Dir],

    case make:all() of
	up_to_date ->
	    io:format("Compilation completed!~n");
	error ->
	    io:format("There were errors during the compilation!~n")
    end.

make_release(RelVsn) ->
    make_basic(),
    
    io:format("Starting creating release ~s~n", [RelVsn]),

    check_rel_dir(RelVsn),

    {ok, Dir} = file:list_dir("lib/"),
    ToLoad = lists:map(fun(AppName) ->
			       [Name | _] = string:tokens(AppName, "-"),
			       list_to_atom(Name)
		       end, Dir),
    [application:load(App) || App <- ToLoad],
    
    Loaded = lists:map(fun({Name, _, Vsn}) ->
			       {Name, Vsn}
		       end, application:loaded_applications()),
    
    create_rel_file(RelVsn, Loaded),
    create_boot_file(RelVsn),
    create_sys_config_file(RelVsn),
    update_start_erl_data_file(RelVsn),
    
    io:format("Release ~s compiled~n", [RelVsn]).

check_rel_dir(RelVsn) ->
    Filename = filename:join(["releases", RelVsn]),

    case file:make_dir(Filename) of 
	ok -> confirm_created(Filename);
	{error, eexist} -> inform_exists(Filename);
	{error, Reason} -> handle_error(Reason)
    end.

create_rel_file(RelVsn, Apps) ->
    Name = "start",
    Filename = filename:join(["releases", RelVsn, Name ++ ".rel"]),
    Version = erlang:system_info(version),
    
    case file:open(Filename, [write]) of
	{ok, Fd} ->
	    ReleaseInfo = {release, {Name, "0.1"}, {erts, Version},
			   Apps},
	    
	    io:format(Fd, "~p.~n", [ReleaseInfo]),
	    file:close(Fd),
	    confirm_created(Filename);
	{error, Reason} ->
	    handle_error(Reason)
    end.

create_boot_file(RelVsn) ->
    systools:make_script("start", [{path, ["releases/" ++ RelVsn, "lib/*/ebin"]}, 
				   {outdir, "releases/" ++ RelVsn}, silent]),
    confirm_created("releases/" ++ RelVsn ++ "/start.script"),
    confirm_created("releases/" ++ RelVsn ++ "/start.boot"),
    
    file:copy("releases/" ++ RelVsn ++ "/start.rel", "start.rel"),
    systools:make_tar("start",[{path, ["releases/" ++ RelVsn]}, 
			       {outdir, "releases/" ++ RelVsn}, silent]),
    file:delete("start.rel"),

    erl_tar:extract("releases/" ++ RelVsn ++ "/start.tar.gz", [compressed]).

create_sys_config_file(RelVsn) ->
    YawsConfig = "config/yaws.config",
    file:copy(code:priv_dir(yaws) ++ "/yaws.conf", YawsConfig),
    confirm_created(YawsConfig),
    
    Filename = "releases/" ++ RelVsn ++ "/sys.config",
    case file:open(Filename, [write]) of
	{ok, Fd} ->
	    Content = [{yaws, [{conf, YawsConfig}]}],
	    io:format(Fd, "~p.~n", [Content]),
	    confirm_created(Filename),
	    file:close(Fd);
	{error, Reason} ->
	    handle_error(Reason)
    end.

update_start_erl_data_file(RelVsn) ->
    Filename = filename:join("releases", "start_erl.data"),

    case file:open(Filename, [write]) of
	{ok, Fd} ->
	    io:format(Fd, RelVsn, []),
	    file:close(Fd),
	    confirm_created(Filename);
	{error, Reason} ->
	    handle_error(Reason)
    end.

clean() ->
    {ok, Dir} = file:list_dir("lib/"),
    clean(Dir).

clean([]) ->
    io:format("Cleaning up completed!~n");
clean(["yaws" ++ _ | Rest]) ->
    clean(Rest);
clean([".svn" | Rest]) ->
    clean(Rest);
clean([App | Rest]) ->
    io:format("Cleaning up ~s~n", [App]),

    {ok, Files0} = file:list_dir(filename:join(["lib", App, "ebin"])),

    Files = lists:map(fun(Name) ->
			      filename:join(["lib", App, "ebin", Name])
		      end, Files0),

    Beams = lists:filter(fun(Name) ->
				 filename:extension(Name) == ".beam"
			 end, Files),

    Remover = fun(File) ->
		      case file:delete(File) of
			  ok ->
			      confirm_deleted(File);
			  {error, Reason} ->
			      handle_error(Reason)
		      end
	      end,
    lists:foreach(Remover, Beams),
    
    clean(Rest).

usage() ->
    io:format("Usage:~n"
	      "bin/compile.erl - basic compile - just compiles the changed files~n"
	      "bin/compile.erl release - the same as bin/compile.erl release 0.1~n"
	      "bin/compile.erl release Vsn - compiles all the sources and create new release - Vsn~n"
	      "bin/compile.erl rel [Vsn] - the same as bin/compile.erl release [Vsn]~n").

confirm_created(Name) ->
    io:format("Element created: ~s~n", [Name]).

inform_exists(Name) ->
    io:format("Element exists, skipping: ~s~n", [Name]).

handle_error(Reason) ->
    io:format("An error has occured: ~s~n", [Reason]).

confirm_deleted(Name) ->
    io:format("Element deleted: ~s~n", [Name]).
