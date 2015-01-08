-module(durga_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
  gen_cowboy:start_http(durga_http_protocol),
  durga_sup:start_link().

stop(_State) ->
  ok.
