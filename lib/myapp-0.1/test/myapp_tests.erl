-module(myapp_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ewts/include/request.hrl").

-compile(export_all).

'404_test'() ->
    Response = ewts_client:request("/404_error"),
    ?assertEqual(Response#response.code, 404).

'501_test'() ->
    Response = ewts_client:request("/blog/post/12/comment/231"),
    ?assertEqual(Response#response.code, 501),
    
    Response2 = ewts_client:request("/some_template.html"),
    ?assertEqual(Response2#response.code, 501).

cookie_test() ->
    Response = ewts_client:request("/widgets/create"),
    Cookie = hd(Response#response.cookies),
    Response2 = ewts_client:request("/widgets/create"),
    Cookie2 = hd(Response2#response.cookies),
    ?assertEqual(Cookie, Cookie2),
    
    ewts_client:clear(),
    Response3 = ewts_client:request("/widgets/create"),
    Cookie3 = hd(Response3#response.cookies),
    ?assertNot(Cookie == Cookie3).
