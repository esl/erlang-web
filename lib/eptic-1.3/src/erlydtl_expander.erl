-module(erlydtl_expander).
-export([process_xml/1]).

process_xml(Mod) ->
    case e_dict:get_state() of
        {ok, Dict} ->
            case Mod:render(Dict) of
                {ok, Html} ->
                    [
                        "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n",
                        wpart_xs:doctype(),
                        "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n",
                        Html,
                        "</html>\n"
                    ]
            end;
        _ ->
            exit(no_dict_attached)
    end.
