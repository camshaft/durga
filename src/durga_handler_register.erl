-module(durga_handler_register).

-export([init/3]).

-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init(_TransportName, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, Opts) ->
  {ok, Req, Opts}.

websocket_handle({binary, Msg}, Req, State) ->
  case decode(Msg) of
    {ok, Res} ->
      {reply, {binary, Res}, Req, State};
    {error, Error} ->
      {reply, {binary, Error}, Req, State}
  end;
websocket_handle(_Data, Req, State) ->
  io:format("Unhandled message ~p~n", [_Data]),
  {ok, Req, State}.

websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _Opts) ->
  durga_manager:unregister_all(self()),
  ok.

decode(Msg) ->
  case annex_marshal_msgpack:decode(Msg) of
    {call, MsgID, Module, Function, Arguments} ->
      case handle_call(Module, Function, Arguments) of
        {ok, Res} ->
          {ok, annex_marshal_msgpack:encode(MsgID, response, Res)};
        _Error ->
          {error, annex_marshal_msgpack:encode(MsgID, error, <<"encountered an error while registering service">>)}
      end;
    {cast, MsgID, _, _, _} ->
      {error, annex_marshal_msgpack:encode(MsgID, error, <<"cast is not supported">>)};
    _Other ->
      io:format("~p~n", [_Other]),
      {error, annex_marshal_msgpack:encode(0, error, <<"could not decode previous message">>)}
  end.

handle_call(<<"services">>, <<"register">>, [Url, Title, Type, Module, Method, Arguments, Response]) ->
  durga_manager:register(self(), Url, Title, Type, Module, Method, Arguments, Response),
  {ok, nil};
handle_call(<<"services">>, <<"unregister">>, [Url, Title, Type, Module, Method, Arguments, Response]) ->
  durga_manager:unregister(self(), Url, Title, Type, Module, Method, Arguments, Response),
  {ok, nil};
handle_call(_, _, _) ->
  {error, notfound}.
