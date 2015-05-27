defmodule Durga.Mixfile do
  use Mix.Project

  def project do
    [app: :durga,
     version: "0.1.0",
     elixir: "~> 1.0",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps]
  end

  def application do
    [
      mod: { Durga, [] },
      applications: [
        :logger,
        :cowboy
      ]
    ]
  end

  defp deps do
    [{:cowboy, "~> 1.0.0"},
     {:gproc, github: "uwiger/gproc", tag: "0.4"},
     {:simple_env, github: "camshaft/simple_env"},]
  end
end
