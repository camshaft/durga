-module(durga_handler_root).

-export([init/3]).

-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-export([handle/2]).
-export([terminate/3]).

init(_TransportName, Req, Opts) ->
  case cowboy_req:header(<<"upgrade">>, Req) of
    {<<"websocket">>, _} ->
      {upgrade, protocol, cowboy_websocket};
    _ ->
      {ok, Req, Opts}
  end.

%% WS

websocket_init(_TransportName, Req, Opts) ->
  {ok, Req, Opts}.

websocket_handle({text, Msg}, Req, State) ->
  {reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
  erlang:start_timer(1000, self(), <<"How' you doin'?">>),
  {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _Opts) ->
  %% gen_event:delete_handler(?MODULE, {?MODULE, self()}, [self()]),
  ok.

%% HTTP

handle(Req, Opts) ->
  {ok, Req2} = cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], <<"{}">>, Req),
  {ok, Req2, Opts}.

terminate(_Reason, _Req, _Opts) ->
  ok.
