defmodule Durga.Handler do
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

  def websocket_init(_, req, opts) do
    {:ok, req, opts}
  end

  def websocket_handle({:binary, msg}, req, state) do
    IO.inspect msg
    {:reply, {:binary, ["Hello, ", msg]}, req, state}
  end

  def websocket_handle(_data, req, state) do
    IO.inspect _data
    {:ok, req, state}
  end

  def websocket_info(_info, req, state) do
    {:ok, req, state}
  end

  def websocket_terminate(_reason, _req, _opts) do
    :ok
  end
end