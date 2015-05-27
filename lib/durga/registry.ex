defmodule Durga.Registry do
  ## TODO figure out global
  scope = :l

  def register(module, function, arity, env \\ "prod") do
    :gproc.reg({:p, unquote(scope), {__MODULE__, {module, function, arity, env}}})
  rescue
    e ->
      e
  end

  def unregister(module, function, arity, env \\ "prod") do
    :gproc.unreg({:p, unquote(scope), {__MODULE__, {module, function, arity, env}}})
  rescue
    e ->
      e
  end

  def list() do
    key = {:p, unquote(scope), {__MODULE__, :"$1"}}
    headpat = {key, :_, :_}
    guards = []
    prod = [:"$1"]
    :gproc.select({unquote(scope), :p}, [{headpat, guards, prod}])
    |> Enum.uniq
  end

  def exec(id, module, function, args, env \\ "prod", sender \\ self()) do
    case :gproc.lookup_values({:p, unquote(scope), {__MODULE__, {module, function, length(args), env}}}) do
      [] ->
        {:error, :undef}
      pids ->
        i = rem(:erlang.phash2(args), length(pids))
        pid = elem(:lists.nth(i + 1, pids), 0)
        send(pid, {:req, id, module, function, args, env, sender})
        :ok
    end
  end
end