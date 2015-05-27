defmodule Durga.Handler do
  defstruct reqs: %{}

  require Logger
  alias Durga.Transport

  def init(_, req, opts) do
    case :cowboy_req.header("upgrade", req) do
      {upgrade, _} when upgrade in ["Websocket", "websocket"] ->
        {:upgrade, :protocol, :cowboy_websocket}
      _ ->
        {:ok, req, opts}
    end
  end

  def handle(req, state) do
    {:ok, req} = :cowboy_req.reply(200, [
      {"content-type", "text/plain; charset=utf-8"}
    ], """
    Welcome, friend.
    """, req)
    {:ok, req, state}
  end

  def terminate(_reason, _req, _state) do
    :ok
  end

  def websocket_init(_, req, _opts) do
    {:ok, req, %__MODULE__{}}
  end

  def websocket_handle({:binary, msg}, req, state) do
    msg
    |> Transport.decode
    |> handle_message(req, state)
  end
  def websocket_handle(msg, req, state) when msg == :ping or is_tuple(msg) and elem(msg, 0) == :ping do
    {:reply, :pong, req, state}
  end
  def websocket_handle(other, req, state) do
    Logger.warn("unhandled message #{inspect(other)}")
    {:ok, req, state}
  end

  def websocket_info({:req, id, module, function, args, env, sender}, req, state) do
    Logger.debug(fn -> "<- req #{id} #{module}:#{function}/#{length(args)}@#{env}" end)
    msg = Transport.encode({:req, id, module, function, args, env})
    reqs = Dict.put(state.reqs, id, sender)
    {:reply, {:binary, msg}, req, %{state | reqs: reqs}}
  end
  def websocket_info({:res, id, res}, req, state) do
    Logger.debug(fn -> "<- res #{id} #{inspect(res)}" end)
    msg = Transport.encode({:res, id, res})
    {:reply, {:binary, msg}, req, state}
  end
  def websocket_info({:error, id, code, error}, req, state) do
    Logger.debug(fn -> "<- error #{id} #{code}:#{inspect(error)}" end)
    msg = Transport.encode({:error, id, code, error})
    {:reply, {:binary, msg}, req, state}
  end
  def websocket_info(_info, req, state) do
    IO.inspect {:info, _info}
    {:ok, req, state}
  end

  def websocket_terminate(_reason, _req, _opts) do
    :ok
  end

  defp handle_message({:register, module, function, arity, env}, req, state) do
    Logger.debug(fn -> "-> register #{module}:#{function}/#{arity}@#{env}" end)
    Durga.Registry.register(module, function, arity, env)
    {:ok, req, state}
  end
  defp handle_message({:list, id}, req, state) do
    entries = Durga.Registry.list()
    |> Enum.map(&:erlang.tuple_to_list/1)
    msg = Transport.encode({:res, id, entries})
    {:reply, {:binary, msg}, req, state}
  end
  defp handle_message({:unregister, module, function, arity, env}, req, state) do
    Logger.debug(fn -> "-> unregister #{module}:#{function}/#{arity}@#{env}" end)
    Durga.Registry.unregister(module, function, arity, env)
    {:ok, req, state}
  end
  defp handle_message({:req, id, module, function, arguments, env}, req, state) do
    Logger.debug(fn -> "-> req #{id} #{module}:#{function}/#{length(arguments)}@#{env}" end)
    case Durga.Registry.exec(id, module, function, arguments, env) do
      :ok ->
        ## TODO set a timeout for the request
        {:ok, req, state}
      {:error, :undef} ->
        msg = Transport.encode({:error, id, 404, "#{module}:#{function}/#{length(arguments)} not registered"})
        {:reply, {:binary, msg}, req, state}
    end
  end
  defp handle_message({:res, id, res}, req, state) do
    Logger.debug(fn -> "-> res #{id} #{inspect(res)}" end)
    response(id, {:res, id, res}, req, state)
  end
  defp handle_message({:error, id, code, error}, req, state) do
    Logger.debug(fn -> "-> error #{id} #{code}:#{inspect(error)}" end)
    response(id, {:error, id, code, error}, req, state)
  end

  defp response(id, res, req, state) do
    reqs = state.reqs
    case Dict.get(reqs, id) do
      pid when is_pid(pid) ->
        send(pid, res)
        reqs = Dict.delete(reqs, id)
        {:ok, req, %{state | reqs: reqs}}
      nil ->
        Logger.error("req {id} missing")
        {:ok, req, state}
    end
  end
end