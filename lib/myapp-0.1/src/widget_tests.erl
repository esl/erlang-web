-module(widget_tests).

-export([dataflow/1, error/2]).
-export([validate/2]).
-export([render/1, save/1, update_item/1, update/1]).

-spec(dataflow/1 :: (atom()) -> list() | tuple(list())).
dataflow(save)   -> [validate];
dataflow(update) -> [validate];
dataflow(_)      -> [].

-spec(error/2 :: (atom(), term()) -> ok).
error(Fun, _) when Fun == save; Fun == update ->
    Err = wpart:fget("__error"),
    Message = "ERROR: Incomplete input or wrong type in form! Reason: " ++ Err,
    wpart:fset("error_message", Message),
    
    NotValidated = wtype_widget_test:prepare_invalid(),
    wpart:fset("__edit", NotValidated),
    
    {template, "widget_test/" ++ 
     if 
	 Fun == save ->
	     "create.html";
	 true ->
	     "update.html"
     end}.

validate(save, _) ->
    validate_tool:validate_cu(widget_test, create);
validate(update, _) ->
    validate_tool:validate_cu(widget_test, update).

render(_) ->
    case wpart:fget("get:form_type") of
	undefined ->
	    wpart:fset("get:form_type", "div");
	_ ->
	    ok
    end,
    {template, "widget_test/create.html"}.

save(Entity) ->
    wtype_widget_test:create(Entity),

    {redirect, "/"}.

update_item([{id, Id}]) ->
    Item = e_db:read(widget_test, list_to_integer(Id)),
    Test = wpart_db:build_record_structure(widget_test, initial, Item),
    
    wpart:fset("__edit", Test),
    wpart:fset("__primary_key", list_to_integer(Id)),

    {template, "widget_test/update.html"}.

update(Entity) ->
    wtype_widget_test:create(Entity),

    {redirect, "/"}.
