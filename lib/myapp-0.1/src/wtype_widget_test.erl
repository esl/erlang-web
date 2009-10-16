-module(wtype_widget_test).

-export([get_record_info/1, install/0]).
-export([create/1]).
-export([prepare_invalid/0]).

-include_lib("myapp/include/widget_test.hrl").

-spec(get_record_info/1 :: (atom()) -> term()). 
get_record_info(widget_test) ->
    record_info(fields, widget_test);
get_record_info(widget_test_types) ->
    #widget_test_types{}.

-spec(create/1 :: (tuple()) -> term()).
create(Entity) ->
    e_db:write(widget_test, Entity).

-spec(prepare_invalid/0 :: () -> tuple()).
prepare_invalid() ->
    wpart_db:build_record_structure(widget_test, initial, 
				    wpart:fget("__not_validated")).

-spec(install/0 :: () -> any()).
install() ->
    mnesia:create_table(widget_test, [{attributes, get_record_info(widget_test)},
				      {disc_copies, [node()]}]).
