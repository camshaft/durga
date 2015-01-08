-module(durga_http_protocol).

-export([init/1]).
-export([middlewares/1]).

init(Opts) ->
  {ok, Opts}.

middlewares(_) ->
  [
    durga_http_router,
    cowboy_handler
  ].
