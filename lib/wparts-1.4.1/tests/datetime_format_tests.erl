-module(datetime_format_tests).

-include_lib("eunit/include/eunit.hrl").

get_time_new_syntax_test() ->
    ?assertEqual("13:37:35", wtype_time:format("HH:NN:SS", {13, 37, 35})),
    ?assertEqual("13:37:35", wtype_time:get_time("HH:NN:SS", {13, 37, 35})).

get_time_old_syntax_test() ->
    ?assertEqual("13:37:35", wtype_time:format("HH:MM:SS", {13, 37, 35})),
    ?assertEqual("13:37:35", wtype_time:get_time("HH:MM:SS", {13, 37, 35})).

validate_new_syntax_test() ->
    ?assertEqual({ok, {13, 37, 35}},
                 wtype_time:validate({[{format, "HH:NN:SS"}], "13:37:35"})).

validate_old_syntax_test() ->
    ?assertEqual({ok, {13, 37, 35}},
                 wtype_time:validate({[{format, "HH:MM:SS"}], "13:37:35"})).

get_date_test() ->
    ?assertEqual("24.07.1986", wtype_date:get_date("DD.MM.YYYY", {1986, 07, 24})).

validate_date_test() ->
    ?assertEqual({ok, {1986, 07, 24}},
                 wtype_date:validate({[{format, "DD.MM.YYYY"}], "24.07.1986"})).
