-module(durga_handler_subscribe).

-export([init/3]).

-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-define(OPTIONS, [
  {allow_atom, pack},
  {format, map}
]).

init(_TransportName, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

%% WS

websocket_init(_TransportName, Req, Opts) ->
  durga_manager:subscribe(self()),
  {ok, Req, Opts}.

websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({update, List}, Req, State) ->
  {reply, {binary, msgpack:pack(List, ?OPTIONS)}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _Opts) ->
  durga_manager:unsubscribe(self()),
  ok.
