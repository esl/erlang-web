-module(ewts_widgets_test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ewts/include/request.hrl").

render_test() ->
    Response = ewts_client:request("/widgets/create", [{"key", "val"}]),
    ?assertEqual(Response#response.code, 200),
    ?assertEqual(ewts:fget("get:key", Response#response.req_dict), "val"),
    ?assert(0 == string:str(Response#response.body, "<table")),
    ?assertNot(0 == string:str(Response#response.body, "autocomplete")),
    ?assert(0 == string:str(Response#response.body, "__primary_key")),
    
    Response2 = ewts_client:request("/widgets/create", [{"form_type", "table"}]),
    ?assertEqual(Response2#response.code, 200),
    ?assertEqual(ewts:fget("get:form_type", Response2#response.req_dict), "table"),
    ?assertNot(0 == string:str(Response2#response.body, "<table")),
    ?assert(0 == string:str(Response2#response.body, "__primary_key")),
    ?assertNot(0 == string:str(Response2#response.body, "autocomplete")).

wrong_save_test() ->
    Post = [{"widget_test_atom", "myapp"},
	    {"widget_test_autocomplete", "test"},
	    {"widget_test_bool", "man"},
	    {"widget_test_csv", "this is a test"},
	    {"widget_test_date", "2001-12-12"},
	    {"widget_test_datetime", "2001-12-12 21:21:21"},
	    {"widget_test_enum", "blue"},
	    {"widget_test_integer", "444"},
	    {"widget_test_multilist", "dog"},
	    {"widget_test_password", "pwd"}],
    Response = ewts_client:request("/widgets/save", [], Post),

    ?assertEqual(Response#response.code, 200),
    ?assert(0 == string:str(Response#response.body, "__primary_key")),
    ?assertNot(0 == string:str(Response#response.body, "this is a test")),
    ?assertNot(undefined == ewts:fget("__not_validated", Response#response.req_dict)),
    ?assertNot(undefined == ewts:fget("__edit", Response#response.req_dict)),
    ?assertEqual("/widgets/save", ewts:fget("__path", Response#response.req_dict)),
    ?assertEqual(widget_tests, ewts:fget("__controller", Response#response.req_dict)).

good_save_test() ->
    ewts_dbg:clear(),
    ewts_dbg:add_case({dispatcher_result, {widget_tests, save, []}}),
    ewts_dbg:add_case({function_response, {widget_tests, dataflow, 1}, [validate]}),
    ewts_dbg:add_case({function_response, {widget_tests, save, 1}, {redirect, "/"}}),
    
    Post = [{"widget_test_atom", "myapp"},
	    {"widget_test_autocomplete", "test"},
	    {"widget_test_bool", "man"},
	    {"widget_test_csv", "this is a test"},
	    {"widget_test_date", "2001-12-12"},
	    {"widget_test_datetime", "2001-12-12 21:21:21"},
	    {"widget_test_enum", "blue"},
	    {"widget_test_integer", "444"},
	    {"widget_test_float", "213123.213213"},
	    {"widget_test_multilist", "dog"},
	    {"widget_test_password", "pwd"},
	    {"widget_test_string", "string"},
	    {"widget_test_text", "text"},
	    {"widget_test_time", "12:21:12"}],
    Response = ewts_client:request("/widgets/save", [], Post),

    ?assertEqual(Response#response.code, 302),
    ?assert(undefined == ewts:fget("__not_validated", Response#response.req_dict)),
    ?assert(undefined == ewts:fget("__edit", Response#response.req_dict)),
    ?assertEqual("/widgets/save", ewts:fget("__path", Response#response.req_dict)),
    ?assertEqual(widget_tests, ewts:fget("__controller", Response#response.req_dict)),
    
    ?assertDbg(ewts_dbg:results(), 3).
%% the same as
%%  {Passed, Failed} = ewts_dbg:results(),
%%  ?assert(Failed == 0),
%%  ?assert(Passed >= 3),
%%  ewts_dbg:clear().

render_update_test() ->
%% done only to become sure we have at least one element in the db
    Post = [{"widget_test_atom", "myapp"},
	    {"widget_test_autocomplete", "test"},
	    {"widget_test_bool", "man"},
	    {"widget_test_csv", "this is a test"},
	    {"widget_test_date", "2001-12-12"},
	    {"widget_test_datetime", "2001-12-12 21:21:21"},
	    {"widget_test_enum", "blue"},
	    {"widget_test_integer", "444"},
	    {"widget_test_float", "213123.213213"},
	    {"widget_test_multilist", "dog"},
	    {"widget_test_password", "pwd"},
	    {"widget_test_string", "string"},
	    {"widget_test_text", "text"},
	    {"widget_test_time", "12:21:12"}],
    ewts_client:request("/widgets/save", [], Post),

    Response = ewts_client:request("/widgets/update/1"),
    ?assertEqual(Response#response.code, 200),
    ?assertEqual(1, ewts:fget("__primary_key", Response#response.req_dict)), 
    ?assertNot(undefined == ewts:fget("__edit", Response#response.req_dict)).

wrong_update_test() ->
    PostCorrect = [{"widget_test_atom", "myapp"},
		   {"widget_test_autocomplete", "test"},
		   {"widget_test_bool", "man"},
		   {"widget_test_csv", "this is a test"},
		   {"widget_test_date", "2001-12-12"},
		   {"widget_test_datetime", "2001-12-12 21:21:21"},
		   {"widget_test_enum", "blue"},
		   {"widget_test_integer", "444"},
		   {"widget_test_float", "213123.213213"},
		   {"widget_test_multilist", "dog"},
		   {"widget_test_password", "pwd"},
		   {"widget_test_string", "string"},
		   {"widget_test_text", "text"},
		   {"widget_test_time", "12:21:12"}],
    ewts_client:request("/widgets/save", [], PostCorrect),
    
    Post = [{"__primary_key", "1"},
	    {"widget_test_time", "wrong time format"}],
    Response = ewts_client:request("/widgets/update", [], Post),
    ?assertEqual(Response#response.code, 200),
    ?assertNot(undefined == ewts:fget("__edit", Response#response.req_dict)).

correct_update_test() ->
    PostCorrect = [{"widget_test_atom", "myapp"},
		   {"widget_test_autocomplete", "test"},
		   {"widget_test_bool", "man"},
		   {"widget_test_csv", "this is a test"},
		   {"widget_test_date", "2001-12-12"},
		   {"widget_test_datetime", "2001-12-12 21:21:21"},
		   {"widget_test_enum", "blue"},
		   {"widget_test_integer", "444"},
		   {"widget_test_float", "213123.213213"},
		   {"widget_test_multilist", "dog"},
		   {"widget_test_password", "pwd"},
		   {"widget_test_string", "string"},
		   {"widget_test_text", "text"},
		   {"widget_test_time", "12:21:12"}],
    ewts_client:request("/widgets/save", [], PostCorrect),

    Post = [{"__primary_key", "1"},
	    {"widget_test_atom", "myapp"},
	    {"widget_test_autocomplete", "correct"},
	    {"widget_test_bool", "man"},
	    {"widget_test_csv", "this is a test"},
	    {"widget_test_date", "2001-12-12"},
	    {"widget_test_datetime", "2001-12-12 21:21:21"},
	    {"widget_test_enum", "blue"},
	    {"widget_test_integer", "444"},
	    {"widget_test_float", "213123.213213"},
	    {"widget_test_multilist", "dog"},
	    {"widget_test_password", "pwd"},
	    {"widget_test_string", "string"},
	    {"widget_test_text", "text"},
	    {"widget_test_time", "12:21:12"}],
    Response = ewts_client:request("/widgets/update", [], Post),
    ?assertEqual(Response#response.code, 302).
