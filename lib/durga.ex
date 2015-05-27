defmodule Durga do
  use Application

  def start(_type, _args) do

    dispatch = :cowboy_router.compile([
      {:_, [
        {'/', Durga.Handler, []}
      ]}
    ])

    port = :simple_env.get_integer('PORT', 7000)
    :cowboy.start_http(Durga.Handler, 100, [port: port], [
      env: [dispatch: dispatch]
    ])

    Durga.Supervisor.start_link()
  end
end
