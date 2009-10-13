-module(ewts).

-export([start_tests/1, fget/2]).

-spec(start_tests/1 :: (string()) -> any()).
start_tests(Outdir) ->
    error_logger:tty(false),
    Modules = lists:concat(lists:map(
                             fun(Application) ->
                                     Result = cover:compile_directory(filename:join(["../../apps/", Application, "src"]),
								      [{i, filename:join(["/../../apps/", Application, "include"])}]),
				     if
					 is_list(Result) ->
					     lists:foldl(fun({ok, M}, Acc) -> [M | Acc];
							    (_, Acc) -> Acc end, [], Result);
					 true ->
					     []
				     end
                             end, get_apps())),
    TestModules = [list_to_atom(filename:basename(F, ".erl")) ||
		      F <- filelib:wildcard(filename:join("../../apps/*/test/", "*.erl"))],
    EUnitResult = eunit:test(TestModules),

    if
	EUnitResult == ok ->
	    TotalPercentage = html_report(Outdir, Modules),
            io:format("All tests passed.  ~B% line coverage.~n", [TotalPercentage]),
	    
            %% Taken from http://idlingspace.com/game/perfect_lemmings/lemmingology_mathe/
            Needed = 70,
            Rescued = TotalPercentage,
            Msg =
                if Rescued == 0 ->
                        "ROCK BOTTOM! I hope for your sake that you nuked that level.";
                   Rescued < (Needed div 2) ->
                        "Better rethink your strategy before you try this level again!";
                   Rescued < (Needed - 5) ->
                        "A little more practice on this level is definitely recommended.";
                   Rescued < (Needed - 1) ->
                        "You got pretty close that time. Now try again for that few percent extra.";
                   Rescued == (Needed - 1) ->
                        "OH NO, So near and yet so far (teehee) Maybe this time.....";
                   Rescued == Needed ->
                        "SPOT ON. You can't get much closer than that. Let's try the next....";
                   Rescued < (Needed + 20), Rescued < 100 ->
                        "That level seemed no problem to you on that attempt. Onto the next....";
                   Rescued < 100 ->
                        "You totally stormed that level! Let's see if you can storm the next...";
                   Rescued == 100 ->
                        "Superb! You rescued every lemming on that level. Can you do it again....?"
                end,
            io:format("~s~n", [Msg]);
	true ->
	    io:format("Wow! Hold your horses mighty cowboy - eunit has failed, hasn't it?~n")
    end,
    init:stop().

-spec(fget/2 :: (string(), term()) -> undefined | term()).
fget(Key0, Dict) ->
    case string:tokens(Key0, ":") of
	[List, Key] ->
	    fget0(List, Key, Dict);
	[Key] ->
	    fget0(Key, Dict)
    end.

-spec(fget0/2 :: (string(), term()) -> undefined | term()).
fget0(Key, Dict) ->
    case dict:find(Key, Dict) of
	{ok, Val} ->
	    Val;
	error ->
	    undefined
    end.

-spec(fget0/3 :: (string(), string(), term()) -> undefined | term()).
fget0(List, Key, Dict) ->
    case fget0(List, Dict) of
	PropList when is_list(PropList) ->
	    case proplists:get_all_values(Key, PropList) of
		[] -> undefined;
		[Value] -> Value;
		Values -> Values  
	    end;
	Result ->
	    Result
    end.

-spec(get_apps/0 :: () -> (list(string()))).
get_apps() ->
    element(2, file:list_dir("../../apps/")) -- ["yaws", "eptic", "eptic_fe", "wpart", 
						 "wparts", "ewts"].

html_report(Path, Modules) ->
    Results = lists:map(fun(Module) ->
				case cover:analyse(Module, module) of
                                    {ok, Result}    -> Result;
                                    {error, Reason} -> exit({invalid_module, Reason})
				end
                        end, Modules),
    case filelib:ensure_dir(filename:join(Path, "index.html")) of
        ok              -> ok;
        {error, Reason} -> exit({invalid_dir, Reason})
    end,
    Files = lists:map(fun(Module) ->
                              File = filename:join(Path, ["mod_", Module, ".html"]),
                              {ok, File} = cover:analyse_to_file(Module, File, [html]),
                              filename:basename(File)
                      end, Modules),
    output_index(Path, lists:zip3(Modules, Files, percentage_per_file(Results))),
    percentage_total(Results).

percentage_total(Results) ->
    {Covered, Total} =
        lists:foldl(
          fun({_Module, {C, NC}}, {Covered, Total}) ->
                  {Covered + C, Total + C + NC}
          end, {0, 0}, Results),
    case Total of
        0 -> 0;
        _ -> round(100 * Covered / Total)
    end.

percentage_per_file(Results) ->
    lists:map(fun({_Module, {Covered, NotCovered}}) ->
                      Total = Covered + NotCovered,
                      round(100 * Covered / Total)
              end, Results).

output_index(Path, Info) ->
    IndexFile = filename:join(Path, "index.html"),
    IoDevice = case file:open(IndexFile, [write]) of
                   {ok, IoD}       -> IoD;
                   {error, Reason} -> exit({invalid_file, Reason})
               end,
    output_header(IoDevice),
    lists:foreach(fun({Module, File, Percentage}) ->
                          io:format(IoDevice, "~s~n", [
						[
							"<li><a href=\"",
							File,
							"\">",
							atom_to_list(Module),
							"</a> Covered: ",
							integer_to_list(Percentage),
							"%",
							"</a>"
						]
                                               ])
                  end, Info),
    output_footer(IoDevice),
    file:close(IoDevice),
    IndexFile.

output_header(IoDevice) ->
    io:format(IoDevice, "<html>~n<head></head><body>", []).

output_footer(IoDevice) ->
    io:format(IoDevice, "</body>~n</html>~n", []).
