-module(tpl_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ewts/include/request.hrl").

-compile(export_all).

tpl_test() ->
    Response = ewts_client:request("/tpl_top_down"),

    ?assertEqual(Response#response.code, 200),
    ?assertNot(string:str(Response#response.body, "Inserted template") == 0).
