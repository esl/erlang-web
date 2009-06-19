-define(ERR(Msg), 
        io_lib:format("<html>\n"
                      "<head></head>\n"
                      "<body>\n"
                      "<h1>An Error Occured:</h1>\n"
                      "<pre>~s</pre>\n"
                      "<hr/>\n<em>Erlang Web 1.3</em>\n"
                      "</body></html>\n", [Msg])).

-define(VALIDATION(Msg), 
        ["<html>\n"
         "<head></head>\n"
         "<body>\n"
         "<h1>Validation violation:</h1>\n"
         "<pre>",Msg,"</pre>\n"
         "<hr/>\n<em>Erlang Web 1.3</em>\n"
         "</body></html>\n"]).

-define(XHTML_HEADER, 
        "<?xml version=\"1.0\"?>\n"
        "<!DOCTYPE html\n"
        "\tPUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n"
        "\t\"http://www.w3.org/TR/xhtml1/DTD/"
        "xhtml1-strict.dtd\">\n").

-define(COOKIE, "eptic_cookie").

-define(ExtendTime_default, (30*60)).
