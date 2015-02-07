defmodule KATT.Mixfile do
  use Mix.Project

  def project do
    [app: :katt,
     version: "1.3.0",
     description: description,
     package: package,
     deps: deps]
  end

  defp deps do
    [{:lhttpc, github: "waisbrot/lhttpc"},
     {:mochijson3, github: "tophitpoker/mochijson3"},
     {:meck, "~> 0.8.2", only: :test, env: :test},
     {:neotoma, github: "seancribbs/neotoma", only: :dev, env: :dev}]
  end

  defp description do
    """
    KATT (Klarna API Testing Tool) is an HTTP-based API testing tool for Erlang.
    """
  end

  defp package do
    [files: ["AUTHORS",
             "Makefile",
             "LICENSE",
             "README.md",
             "doc",
             "erlang.mk",
             "include",
             "package.exs",
             "priv",
             "src"],
     licenses: ["Apache 2.0"],
     links: %{"GitHub" => "https://github.com/for-GET/katt"}]
   end
end
