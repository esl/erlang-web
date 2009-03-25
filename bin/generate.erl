#!/usr/bin/env escript

-include_lib("kernel/include/file.hrl").

main(["model", App, Name]) ->
    generate_model(App, Name),
    generate_controller(App, Name);
main(["controller", App, Name]) ->
    generate_controller(App, Name);
main(_) ->
    help().

generate_model(App, Name) ->
    ok.

generate_controller(App, Name) ->
    CName = string:strip(io:get_line("Name of the controller: "), both, 10).
