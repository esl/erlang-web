%% The contents of this file are subject to the Erlang Web Public License,
%% Version 1.0, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Web Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang-consulting.com/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Erlang Training & Consulting
%% Ltd. Portions created by Erlang Training & Consulting Ltd are Copyright 2009,
%% Erlang Training & Consulting Ltd. All Rights Reserved.
-module(ewts).

-export([start/0]).
-export([start_tests/1, fget/2]).

-spec(start/0 :: () -> any()).
start() ->
    application:start(ewts).

-spec(start_tests/1 :: (string()) -> any()).
start_tests(Outdir) ->
    error_logger:tty(false),
    Modules = lists:concat(lists:map(
                             fun(Application) ->
                                     Result = cover:compile_directory(filename:join(["lib", Application, "src"]),
								      [{i, filename:join(["lib", Application, "include"])}]),
				     
				     FilesTest = filelib:wildcard(filename:join(["lib", Application, "test", "*erl"])),
				     make:files(FilesTest, [{outdir, filename:join(["lib", Application, "test"])}, 
							    {i, filename:join(["lib", Application, "include"])}]),

				     if
					 is_list(Result) ->
					     lists:foldl(fun({ok, M}, Acc) -> [M | Acc];
							    (_, Acc) -> Acc end, [], Result);
					 true ->
					     []
				     end
                             end, get_apps())),
    TestModules = [list_to_atom(filename:basename(F, ".erl")) ||
		      F <- filelib:wildcard("lib/*/test/*.erl")],
    EUnitResult = eunit:test(TestModules),

    if
	EUnitResult == ok ->
	    TotalPercentage = html_report(Outdir, Modules),
            io:format("EWTS: All tests passed.  ~B% line coverage.~n", [TotalPercentage]),
	    
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
            io:format("EWTS: ~s~n", [Msg]);
	true ->
	    io:format("EWTS: Wow! Hold your horses mighty cowboy - eunit has failed, hasn't it?~n")
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
    filter(
      element(2, file:list_dir("lib")), []).

-spec(filter/2 :: (list(string()), list(string())) -> list(string())).
filter(["yaws" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["eptic" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["wpart-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["wparts-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["ewts-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["ewgi-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["kernel-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["stdlib-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["mnesia-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["inets-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["mochiweb-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["erlydtl-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["crypto-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["ssl-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter(["sasl-" ++ _ | Rest], Acc) ->
    filter(Rest, Acc);
filter([App | Rest], Acc) ->
    filter(Rest, [App | Acc]);
filter([], Acc) ->
    Acc.

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
