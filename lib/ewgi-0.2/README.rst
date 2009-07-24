EWGI, an Erlang webserver interface specification
=================================================

EWGI (pronounced `you-ghee`) is a specification designed to allow web
applications written in Erlang to run on any supported server.  It
also makes developing web applications simpler and more flexible by
providing a common mechanism for reusing components.  It was inspired
by Python's `PEP 333`_ and provides similar functionality to other
projects such as Ruby's `Rack`_ .

"Hello world!" MochiWeb example
-------------------------------

This sample application simply responds with 200 OK and a
``text/plain`` entity body of ``Hello world!`` for all requests.

Cheers to `Geoff Cant`_ for writing this example.

#. Grab the latest ewgi and `MochiWeb`_ source trees.

   ::
  
    $ git clone git://github.com/skarab/ewgi.git
    $ svn checkout http://mochiweb.googlecode.com/svn/trunk/ \
      mochiweb-read-only

#. Compile them.

   ::
  
    $ (cd ewgi/ && make)
    $ (cd mochiweb-read-only/ && make)

#. Create a file called ``ewex_web.erl``

   ::
  
    -module(ewex_web).
    
    -export([start/0,stop/0,
             loop/1,simple_app/1]).
    
    start() ->
        mochiweb_http:start([{name, ewex}, {loop, fun ?MODULE:loop/1},
                             {ip, "127.0.0.1"}, {port, 8889}]).
    
    stop() ->
        mochiweb_http:stop(ewex).
    
    loop(Req) ->
        Mod = ewgi_mochiweb:new(fun ?MODULE:simple_app/1),
        Mod:run(Req).
   
    simple_app({ewgi_context, Request, _Response}) ->
        ResponseHeaders = [{"Content-type", "text/plain"}],
        Response = {ewgi_response, {200, "OK"}, ResponseHeaders,
                    [<<"Hello world!">>], undefined},
        {ewgi_context, Request, Response}.

#. Compile it

   ::
  
   $ erlc ewex_web.erl

#. Run

   ::
  
    $ erl -pa ewgi/ebin/ -pa mochiweb-read-only/ebin/ -eval \
    'ewex_web:start(), receive done -> done end.'

#. Point your browser to ``http://127.0.0.1:8889/`` (type ``halt().``
   in the Erlang shell when you want to stop)


The even shorter inets example
------------------------------

::

 $ git clone git://github.com/skarab/ewgi.git && (cd ewgi/ && make \
   && erl -pa ebin/ -eval 'application:start(inets)' \
   -eval 'application:set_env(ewgi, app_module, ewgi_test)' \
   -eval 'application:set_env(ewgi, app_function, testapp)' \
   -eval 'inets:start(httpd, [{port, 8889},
                              {server_name, "ewgi"},
                              {server_root, "."},
                              {document_root, "."},
                              {modules, [ewgi_inets]}])')

Middleware components
---------------------

The real power of the EWGI interface specification is the ability to
compose applications so that requests and responses can be modified by
reusable components.  For example, the specification includes an
example middleware component which converts all text responses to
upper-case.

Advantages
----------

* Applications are `server independent.`
* Middleware components can be reused.
* Applications have a clean, functional interface.

Reference implementations
-------------------------

The current server reference implementations include:

* `MochiWeb`_
* `Yaws`_
* `inets`_

.. _PEP 333:
    http://www.python.org/dev/peps/pep-0333/
.. _Rack:
    http://rack.rubyforge.org/
.. _MochiWeb:
    http://code.google.com/p/mochiweb/
.. _Yaws:
    http://yaws.hyber.org/
.. _inets:
    http://erlang.org/doc/apps/inets/http_server.html
.. _Geoff Cant:
    http://github.com/archaelus/
