#!/usr/bin/env escript

main([]) ->
    {Name, Vsn} = get_description(),

    create_new_application(Name, Vsn),
    update_makefile(Name, Vsn).

get_description() ->
    Name = string:strip(io:get_line("Name of your application: "), both, 10),
    Vsn0 = string:strip(io:get_line("Version of your application [0.1]: "), both, 10),

    Vsn = case Vsn0 of
	      V when length(V) > 0 ->
		  case catch list_to_float(V) of
		      F when is_float(F) ->
			  V;
		      _ ->
			  case catch list_to_integer(V) of
			      I when is_integer(I) ->
				  V ++ ".0";
			      _ ->
				  "0.1"
			  end
		  end;
	      _ ->
		  "0.1"
	  end,
    
    {Name, Vsn}. 

create_new_application(Name, Vsn) ->
    Filename = filename:join("lib", Name ++ "-" ++ Vsn),

    Creator = fun(X) ->
		      case file:make_dir(X) of 
			  ok -> confirm_created(X);
			  {error, eexist} -> inform_exists(X);
			  {error, Reason} -> handle_error(Reason)
		      end
	      end,

    Creator(Filename),
    
    AppDirs = ["doc", "ebin", "include", "priv", "src"],
    AppDirsComplete = lists:map(fun(Name0) ->
					filename:join(Filename, Name0)
				end, AppDirs),
    lists:foreach(Creator, AppDirsComplete).

update_makefile(Name, Vsn) ->
    Filename = filename:join("lib", Name ++ "-" ++ Vsn),

    case file:open("Emakefile", [append]) of
	{ok, Fd} ->
	    Entry = {list_to_atom(filename:join([Filename, "src", "*"])),
		     [{outdir, filename:join([Filename, "ebin"])},
		      {i, filename:join([Filename, "include"])},
		      debug_info,
		      strict_record_tests,
		      netload]},

	    io:format(Fd, "~n~p.~n", [Entry]),
	    confirm_updated("Emakefile"),
	    
	    file:close(Fd);
	{error, Reason} ->
	    handle_error({"Emakefile", Reason})
    end.

confirm_created(Name) ->
    io:format("Element created: ~s~n", [Name]).

confirm_updated(Name) ->
    io:format("Element updated: ~s~n", [Name]).

inform_exists(Name) ->
    io:format("Element exists, skipping: ~s~n", [Name]).

handle_error(Reason) ->
    io:format("An error has occured: ~s~n", [Reason]).
