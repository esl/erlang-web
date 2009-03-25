#!/usr/bin/env escript

main([]) ->
    prepare_paths(),
    make:all().

prepare_paths() ->
    {ok, Libs} = file:list_dir("lib"),
    
    lists:foreach(fun(Lib) ->
			  Path = filename:join(["lib", Lib, "ebin"]),
			  code:add_path(Path)
		  end, lists:delete(".svn", Libs)).
