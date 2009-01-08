#!/usr/bin/env escript

-include_lib("kernel/include/file.hrl").

main([]) ->
    Name = string:strip(io:get_line("Name of the tarball: "), both, 10),
    Url = string:strip(io:get_line("SVN address: "), both, 10),
    main([Name, Url]);
main([Name, URL]) ->
    Path = export_svn(URL, Name),
    generate_pdf(Name, Path),

    Tarball = create_tarball(Name, Path),
    calculate_checksum(Tarball),
    
    clean_up(Path).

export_svn(URL, Name) ->
    Path = filename:join(["/tmp", 
			  pid_to_list(self()),
			  Name]),
    file:make_dir(filename:join("/tmp",  
				pid_to_list(self()))),

    os:cmd(io_lib:format("svn export ~p ~p", [URL, Path])),
    
    message("SVN version exported."),
    
    Path.

generate_pdf(Name, Path) ->
    DocPath = filename:join(Path, "doc"),
    PDFName = filename:join(DocPath, Name ++ ".pdf"),

    os:cmd(io_lib:format("make -C ~p", [DocPath])),
    
    file:rename(filename:join(DocPath, "skeleton.pdf"),
		PDFName),
    message("PDF generated"),
    
    clean_tex(DocPath, PDFName).

clean_tex(Path, PDF) ->
    {ok, Dir} = file:list_dir(Path),
    lists:foreach(fun(F) ->
			  delete_file(filename:join(Path, F), PDF)
		  end, Dir),
    
    message("PDF cleaning up completed").

delete_file(PDF, PDF) ->
    ok;
delete_file(File, PDF) ->
    {ok, FileInfo} = file:read_file_info(File),
    case FileInfo#file_info.type of
	directory ->
	    {ok, Dir} = file:list_dir(File),
	    lists:foreach(fun(F) ->
				  delete_file(filename:join(File, F), PDF)
			  end, Dir),
	    
	    file:del_dir(File);
	_ ->
	    file:delete(File)
    end.

create_tarball(Name, Path) ->
    Result = Name ++ ".tar.gz",

    os:cmd(io_lib:format("tar -czf ~p ~p", [Result, Path])),
    
    message("Creating tar.gz completed"),
    Result.

calculate_checksum(Tarball) ->
    Md5 = hd(string:tokens(os:cmd("md5sum " ++ Tarball), " ")),
    Sha1 = hd(string:tokens(os:cmd("sha1sum " ++ Tarball), " ")),
    
    io:format("MD5: ~s~nSHA1: ~s~n", [Md5, Sha1]). 

clean_up(Path) ->
    {ok, Dir} = file:list_dir(Path),
    lists:foreach(fun(F) ->
			  delete_file(filename:join(Path, F), "not used")
		  end, Dir),

    message("Cleaning up completed").

message(Msg) ->
    io:format("~s~n", [Msg]).
