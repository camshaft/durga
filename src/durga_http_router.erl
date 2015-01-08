-module(durga_http_router).

-compile(inline).
-compile({parse_transform, cowboy_compiled_router}).

-get({"/", durga_handler_root}).
-get({"/register", durga_handler_register}).
-get({"/subscribe", durga_handler_subscribe}).
