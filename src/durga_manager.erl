-module(durga_manager).
-behaviour(gen_server).

-export([start_link/0]).

-export([register/8]).
-export([unregister/8]).
-export([unregister_all/1]).
-export([subscribe/1]).
-export([unsubscribe/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
  services,
  subs
}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register(Pid, Env, Url, Type, Module, Method, Arguments, Response) ->
  gen_server:call(?MODULE, {register, {Pid, {Url, Env, Type, Module, Method, Arguments, Response}}}).

unregister(Pid, Env, Url, Type, Module, Method, Arguments, Response) ->
  gen_server:call(?MODULE, {unregister, {Pid, {Url, Env, Type, Module, Method, Arguments, Response}}}).

unregister_all(Pid) ->
  gen_server:call(?MODULE, {unregister_all, Pid}).

subscribe(Pid) ->
  gen_server:call(?MODULE, {subscribe, Pid}).

unsubscribe(Pid) ->
  gen_server:call(?MODULE, {unsubscribe, Pid}).

init([]) ->
  {ok, #state{
    services = ets:new(services, [bag]),
    subs = ets:new(subscriptions, [set])
  }}.

handle_call({register, Obj}, _From, State = #state{services = Services}) ->
  true = ets:insert(Services, Obj),
  self() ! changed,
  {reply, ok, State};
handle_call({unregister, Obj}, _From, State = #state{services = Services}) ->
  true = ets:delete_object(Services, Obj),
  self() ! changed,
  {reply, ok, State};
handle_call({unregister_all, Pid}, _From, State = #state{services = Services}) ->
  true = ets:delete(Services, Pid),
  self() ! changed,
  {reply, ok, State};
handle_call({subscribe, Pid}, _From, State = #state{services = Services, subs = Subs}) ->
  true = ets:insert(Subs, {Pid}),
  Pid ! {update, format(Services)},
  {reply, ok, State};
handle_call({unsubscribe, Pid}, _From, State = #state{subs = Subs}) ->
  true = ets:delete(Subs, {Pid}),
  {reply, ok, State};
handle_call(_Other, _From, State) ->
  {reply, {error, notimplemented}, State}.

handle_cast(_, State) ->
  {noreply, State}.

handle_info(changed, State = #state{services = Services, subs = Subs}) ->
  FormattedServices = format(Services),
  ets:foldl(fun({Sub}, _) ->
    Sub ! {update, FormattedServices},
    ok
  end, ok, Subs),
  {noreply, State};
handle_info(_Message, State) ->
  io:format("Generic info handler: '~p' '~p'~n",[_Message, State]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVersion, _State, _Extra) ->
  {ok, _State}.

format(Tab) ->
  sets:to_list(sets:from_list(ets:foldl(fun({_, {Url, Env, Type, Module, Method, Arguments, Response}}, Acc) ->
    [#{
      url => Url,
      env => Env,
      type => Type,
      module => Module,
      method => Method,
      arguments => Arguments,
      response => Response
    }|Acc]
  end, [], Tab))).
