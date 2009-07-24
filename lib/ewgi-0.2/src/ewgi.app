{application, ewgi,
 [{description, "Erlang Webserver Gateway Interface"}
  ,{vsn, "0.2"}
  ,{applications, [kernel, stdlib]}
  ,{env, []}
  ,{modules, [ewgi_api
              ,ewgi_application
              ,ewgi_mochiweb
              ,ewgi_yaws
             ]}
 ]}.
