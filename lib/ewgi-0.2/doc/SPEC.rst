Abstract
========

This document specifies a proposed standard interface between web
servers and Erlang web applications or frameworks to promote web
application portability across a variety of web server
implementations.

This EEP is originally based on the Python Web Server Gateway
Interface v1.0 (`PEP 333`_).

Rationale
=========

At the time of writing, there is no standard way for Erlang
applications to interact with a web server or HTTP toolkit.  Many
other languages (e.g. Python and Ruby) have dedicated significant
research towards developing robust standards for web applications.  In
order for developers interesting in using Erlang to build web
applications, such standards are important to encourage the reuse of
code dealing with common HTTP problems such as cookies, sessions, and
URL routing.

Specification Overview
======================

The EWGI interface has two sides: the "server" or "gateway" side, and
the "application" or "framework" side.  The server side invokes a
function or module (the "application") that is provided by the
application side.  The specifics of how that function or module is
provided are up to the server or gateway.  It is assumed that some
servers or gateways will require an application's deployer to write a
some code to create an instance of the server or gateway, and supply
it with the application.  Other servers and gateways may use
configuration files or other mechanisms to specify where an
application should be obtained.

In addition to "pure" servers/gateways and applications/frameworks, it
is also possible to create "middleware" components that implement both
sides of this specification.  Such components act as an application to
their containing server, and as a server to a contained application.
They can be used to provide extended APIs, content transformations,
navigation, and other useful functions.

Hot code reloading
==================

It is important to note that one of the core features of the Erlang
runtime system, hot code reloading, may be affected by the use of
first-class functions.  This specification does not deal directly with
the problems associated with hot code reloading and maintains that it
is the responsibility of the server and application developers to
implement the desired release behaviour.

The Application/Framework Side
==============================

The application is simply a function that accepts a single 3-tuple
argument.  Applications MUST be able to be invoked more than once, as
virtually all servers/gateways will make such repeated requests.  The
function should return a similarly-structured 3-tuple argument.  The
3-tuple may be defined by a record for convenience, but this is not
required.  The first element of the context tuple MUST be the atom
``'ewgi_context'``.

`Note: although we refer to it as an "application", this should not be
construed to mean that application developers will use EWGI as a web
programming API!  It is assumed that application developers will
continue to use high-level framework services to develop their
applications.  EWGI is a tool for framework and server developers, and
is not necessarily intended to directly support application developers
as "yet another web framework."`

Here is an example of an application::

    simple_app({ewgi_context, Request, _Response}) ->
        StatusCode = 200,
        ReasonPhrase = "OK",
        Status = {StatusCode, ReasonPhrase},
        ResponseHeaders = [{"Content-type", "text/plain"}],
        Body = [<<"Hello world!">>],
        Response = {ewgi_response, Status,
                    ResponseHeaders, Body, undefined},
        {ewgi_context, Request, Response}.

As stated above, a record may be used for convenience::

    -record(ewgi_context, {
              request,
              response
             }).

The Server/Gateway Side
=======================

The server or gateway invokes the application callable once for each
request it receives from an HTTP client that is directed at the
application.

`An example server using the MochiWeb HTTP toolkit is provided with
the EWGI reference implementation.`

Middleware: Components "that Play Both Sides"
=============================================

Note that a single object may play the role of a server with respect
to some application(s), while also acting as an application with
respect to some server(s).  Such "middleware" components can perform
such functions as:

* Routing a request to different application objects based on the
  target URL, after rewriting the ``Request`` accordingly.

* Allowing multiple applications or frameworks to run side-by-side in
  the same process

* Load balancing and remote processing by forwarding requests and
  responses over a network

* Content postprocessing, such as applying XSL stylesheets

The presence of middleware in general is transparent to both the
"server/gateway" and the "application/framework" sides of the
interface, and should require no special support.  A user who desires
to incorporate middleware into an application simply provides the
middleware component to the server as if it were an application and
configures the middleware component to invoke the application as if
the middleware component were a server.  Of course, the "application"
that the middleware wraps may in fact be another middleware component
wrapping another application and so on, creating what is referred to
as a "middleware stack" or "pipeline."

For the most part, middleware must conform to the restrictions and
requirements of both the server and application sides of EWGI.  In
some cases, however, requirements for middleware are more stringent
than for a "pure" server or application, and these points will be
noted in the specification.

Following is an example which naively converts the output of an
application to uppercase::

    get_upcase_mw(A) when is_function(A, 1) ->
        F = fun(Ctx) ->
                    {ewgi_context, Req, Rsp} = A(Ctx),
                    Body = case element(4, Rsp) of
                               Body0 when is_function(Body0, 0) ->
                                   upcase_chunks(Body0);
                               Body0 when is_list(Body0) ->
                                   upcase_iolist(Body0);
                               Body0 when is_binary(Body0) ->
                                   upcase_binary(Body0)
                           end,
                    {ewgi_context, Req, setelement(4, Rsp, Body)}
            end,
        F.
    
    %% Lazily wrap a stream
    upcase_chunks(F0) ->
        F = fun() ->
                    case F0() of
                        {H, T} ->
                            {upcase_iolist(H), upcase_chunks(T)};
                        {} ->
                            {}
                    end
            end,
        F.
    
    upcase_binary(Bin) when is_binary(Bin) ->
        list_to_binary(string:to_upper(binary_to_list(Bin))).
    
    upcase_iolist(L) ->
        lists:map(fun(A) when is_integer(A) ->
                          string:to_upper(A);
                     (A) when is_binary(A) ->
                          upcase_binary(A);
                     (A) when is_list(A) ->
                          upcase_iolist(A)
                  end, L).

Specification Details
=====================

The application callable must accept one 3-tuple argument.  For the
sake of illustration, we have named the second and third elements of
this tuple ``request`` and ``response``, and the specification shall
refer to them by those names.  A server or gateway must invoke the
callable by passing the tuple argument (e.g. by calling ``Result =
Application({ewgi_context, Request, Response})`` as shown above).

Request
-------

The ``Request`` parameter is a tuple containing various CGI-influenced
environment variables.  This term must be an 21-tuple, and the
application is allowed to modify the ``Request`` in any way it desires
(except for HTTP header restrictions outlined later).  Element 5 of
the tuple must itself be a 6-tuple including certain EWGI-required
terms (described in a later section), and may also include
server-specific extension variables by making use of the final element
(a bag or multiset).  Element 7 of the tuple must itself be a 8-tuple
including certain commonly-encountered HTTP headers and a dictionary
for additional variables. The following records may be used for
convenience::

    -record(ewgi_spec, {
              read_input,
              write_error,
              url_scheme,
              version,
              data % set
             }).
    
    -record(ewgi_http_headers, {
              http_accept,
              http_cookie,
              http_host,
              http_if_modified_since,
              http_user_agent,
              http_x_http_method_override,
              other % multiset
             }).
    
    -record(ewgi_request, {
              auth_type,
              content_length,
              content_type,
              ewgi=#ewgi_spec{},
              gateway_interface,
              http_headers=#ewgi_http_headers{},
              path_info,
              path_translated,
              query_string,
              remote_addr,
              remote_host,
              remote_ident,
              remote_user,
              remote_user_data,
              request_method,
              script_name,
              server_name,
              server_port,
              server_protocol,
              server_software
             }).

EWGI request variables
''''''''''''''''''''''

The ``Request`` tuple is required to contain these CGI environment
variables, as originally defined by the `Common Gateway Interface
specification`_.

``auth_type``: (Element 2) The type of authentication provided or
``'undefined'`` if absent.

``content_length``: (Element 3) The contents of any ``Content-Length``
fields in the HTTP request. May be empty or ``'undefined'``.

``content_type``: (Element 4) The contents of any ``Content-Type``
fields in the HTTP request. May be empty or ``'undefined'``.

``ewgi``: (Element 5) See section below

``gateway_interface``: (Element 6) The gateway interface and revision
used. Should be ``EWGI/1.1`` for this version of the specification.

``http_headers``: (Element 7) See section below

``path_info``: (Element 8) The remainder of the request URL's "path",
designating the virtual "location" of the request's target within the
application.  This may be an empty string, if the request URL targets
the application root and does not have a trailing slash.

``path_translated``: (Element 9) The path as may be translated by the
server to a physical location.

``query_string``: (Element 10) The portion of the request URL that
follows the ``"?"``, if any. May be empty or ``'undefined'``.

``remote_addr``: (Element 11) The remote IP address of the client
issuing the request

``remote_host``: (Element 12) The remote hostname of the client
issuing the request. May be empty or ``'undefined'``.

``remote_ident``: (Element 13) If the server supports `RFC 931`_
identification, this variable may be set to the remote user
name. Should only be used for logging purposes.

``remote_user``: (Element 14) If authentication is supported by the
server (or middleware), this should be set to the authenticated
username.

``remote_user_data``: (Element 15) Any additional data provided by the
authentication mechanism.

``request_method``: (Element 16) An atom or string describing the HTTP
request method.  Common methods MUST be atoms and include
``'OPTIONS'``, ``'GET'``, ``'HEAD'``, ``'POST'``, ``'PUT'``,
``'DELETE'``, ``'TRACE'``, and ``'CONNECT'``.  A value is always
required and it MUST NOT be an empty string.

``script_name``: (Element 17) The initial portion of the request URL's
"path" that corresponds to the application object, so that the
application knows its virtual "location".  This may be an empty
string, if the application corresponds to the "root" of the server.

``server_name``, ``server_port``: (Element 18,19) When combined with
``script_name`` and ``path_info``, these variables can be used to
complete the URL.  Note, however, that ``http_host``, if present,
should be used in preference to ``server_name`` for reconstructing the
request URL. ``server_name`` and ``server_port`` can never be empty
strings, and so are always required.

``server_protocol``: (Element 20) The version of the protocol the
client used to send the request. Typically this will be something like
``"HTTP/1.0"`` or ``"HTTP/1.1"``and may be used by the application to
determine how to treat any HTTP request headers.  (This variable
should probably be called ``request_protocol``, since it denotes the
protocol used in the request, and is not necessarily the protocol that
will be used in the server's response.  However, for compatibility
with CGI we have to keep the existing name).

``server_software``: (Element 21) The name and revision of the server
software answering the request.

EWGI-specification parameters
'''''''''''''''''''''''''''''

``read_input``: (Element 2) A 2-arity function which takes a
``Callback`` 1-arity function and a ``Size`` non-zero integer.  The
``Callback`` function will be called with chunks of data in the form
``{data, Bin}`` where ``Bin`` is a binary.  At the end of reading, the
``Callback`` function will be called with ``eof`` as its argument.
The supplied function should return another function of the same kind.

``write_error``: (Element 3) A 1-arity function which takes an
``iolist`` and writes to the server-defined error log mechanism
(usually ``error_logger``).

``url_scheme``: (Element 4) A string representing the "scheme" portion
of the URL at which the application is being invoked. Normally, this
will have the value ``"http"`` or ``"https"`` where appropriate.

``version``: (Element 5) The tuple ``{1,1}``, representing EWGI major
version 1, minor version 1.

``data``: (Element 6) A dictionary (implemented by the OTP module
``gb_trees``) which can be used for server or application-specific
data to be included with the request.  A common use for this
dictionary is in configuring higher-level web frameworks or providing
cached data. Additionally, a server or gateway should attempt to
provide as many other CGI variables as are applicable.  In addition,
if SSL is in use, the server or gateway should also provide as many of
the `Apache SSL environment variables`_ as are applicable, such as
``https`` and ``ssl_protocol``.  Note, however, that an application
that uses any CGI variables other than the ones listed above are
necessarily non-portable to web servers that do not support the
relevant extensions. An EWGI-compliant server or gateway should
document what variables it provides, along with their definitions as
appropriate.  Applications should check for the presence of any
variables they require, and have a fallback plan in the event such a
variable is ``'undefined'``.

HTTP headers
''''''''''''

EWGI provides a tuple with commonly-used HTTP request headers to
optimise retrieval.  Each of the values is a list of 2-tuples of the
form {``FieldName``, ``FieldValue``}.  Servers MUST preserve the order
of headers as they are given in the request.  Servers SHOULD preserve
the case of the ``FieldName`` values.

``http_accept``: (Element 2) The ``Accept:`` header

``http_cookie``: (Element 3) The ``Cookie:`` header

``http_host``: (Element 4) The ``Host:`` header

``http_if_modified_since``: (Element 5) The ``If-Modified-Since:``
header

``http_user_agent``: (Element 6) The ``User-Agent:`` header

``http_x_http_method_override``: (Element 7) The
``X-Http-Method-Override:`` header.  While not part of the HTTP 1.1
specification, this header can be used to overcome a common browser
limitation which prevents browsers from sending a ``PUT`` or
``DELETE`` request to a URI.

``other``: (Element 8) A multiset (implemented by the OTP module
``gb_trees``) which contains all other HTTP request headers. The keys
of the dictionary should be lower-case representations of the header
names and the values should be a list of tuples of the form
{``HeaderName``, ``HeaderValue``}.  Servers SHOULD attempt to preserve
the original case of header names in the tuple list.

Notes
'''''

Missing variables (where allowed, such as ``remote_user`` when no
authentication has occurred) should be defined by the atom
``'undefined'``.  Also note that CGI-defined variables must be strings
if they are defined.  It is a violation of this specification for a
CGI variable's value to be of any type other than ``string`` or the
``'undefined'`` atom.

Response
--------

The ``Response`` parameter is a 5-tuple of the form ``{ewgi_response,
{StatusCode, ReasonPhrase}, HeaderList, MessageBody, Error}``. and A
convenient record definition is::

    -record(ewgi_response, {
              status={200, "OK"},
              headers=[],
              message_body,
              err
             }).

Status Code
'''''''''''

The ``StatusCode`` parameter should be a 3-digit integer corresponding
to the HTTP status code as defined in the HTTP specification (See `RFC
2616, Section 6.1.1`_ for more information).  For example, ``200``
corresponds to a successful request.

Reason Phrase
'''''''''''''

The ``ReasonPhrase`` parameter is intended to be a human readable
representation of ``StatusCode`` and should be a string or binary.

Headers
'''''''

``Headers`` is a list of ``{HeaderName, HeaderValue}`` tuples
describing the HTTP response headers.

Each ``HeaderName`` must be a valid HTTP header field-name (as defined
by `RFC 2616, Section 4.2`_), without a trailing colon or bother
punctuation.  Note: ``HeaderName`` is case insensitive, but should be
lower-case for optimising comparisons. (A reminder for server/gateway
authors: be sure to take that into consideration when examining
application-supplied headers).

Each ``HeaderValue`` must not include any control characters,
including CR or LF, in any position.

In general, the server or gateway is responsible for ensuring that
correct headers are sent to the client: if the application omits a
header required by HTTP (or other relevant specifications that are in
effect), the server or gateway must add it.  For example, the HTTP
``Date:`` and ``Server:`` headers would normally be supplied by the
server or gateway.

Applications and middleware are forbidden from using HTTP/1.1
"hop-by-hop" features or headers, any equivalent features in HTTP/1.0,
or any headers that would affect the persistence of the client's
connection to the web server.  These features are the exclusive
province of the actual web server, and a server or gateway should
consider it a fatal error for an application to attempt sending them,
and raise an error if they are supplied.

For example::

    [{"content-type", "application/json"}, {"etag", "8a920bc001df"}]

Message Body
''''''''''''

The ``MessageBody`` parameter is either an ``iolist`` or a "stream,"
which is a lazy, recursive list-like structure.  A stream is a
zero-arity function which returns either the empty tuple ``{}`` or a
2-tuple of the form ``{Head, Tail}`` where ``Head`` is an ``iolist``
and ``Tail`` is another stream.  Servers may choose to transmit
message bodies represented by a stream using the chunked transfer
encoding.  However, the server or gateway must transmit ``iolist``s to
the client in an unbuffered fashion, completing the transmission of
each ``iolist`` before requesting another one.  (In other words,
applications should perform their own buffering).

The server or gateway should not alter the ``iolist`` returned by the
application in any way.  The application is responsible for ensuring
that the ``iolist`` to be written is in a format suitable for the
client.  However, the server or gateway may apply HTTP transfer
encodings or perform other transformations for the purpose of
implementing HTTP features such as byte-range transmission.

EWGI Reference Implementation
=============================

The EWGI reference implementation includes an API module ``ewgi_api``
which defines helper functions to access and modify the EWGI context,
parse query strings, etc.  It also includes a module
``ewgi_application`` which contains convenience functions for dealing
with application functions as well as sample middleware components.
An include file (``include/ewgi.hrl``) is also provided, which
contains macros for standard HTTP status values and the convenience
record definitions.  These may be used to help development of servers
and applications, but should not be required.

Copyright
=========

This document has been placed in the public domain.

.. _PEP 333:
    http://www.python.org/dev/peps/pep-0333/

.. _Common Gateway Interface specification:
    http://cgi-spec.golux.com/draft-coar-cgi-v11-03.txt

.. _Apache SSL environment variables:
    http://www.modssl.org/docs/2.8/ssl_reference.html#ToC25

.. _RFC 2616, Section 6.1.1:
    http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6.1.1

.. _RFC 931:
    http://www.faqs.org/rfcs/rfc931.html

.. _RFC 2616, Section 4.2:
    http://www.w3.org/Protocols/rfc2616/rfc2616-sec4.html#sec4.2
