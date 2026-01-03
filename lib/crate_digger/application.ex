defmodule CrateDigger.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      CrateDiggerWeb.Telemetry,
      CrateDigger.Repo,
      {DNSCluster, query: Application.get_env(:crate_digger, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: CrateDigger.PubSub},
      # Start a worker by calling: CrateDigger.Worker.start_link(arg)
      # {CrateDigger.Worker, arg},
      # Start to serve requests, typically the last entry
      CrateDiggerWeb.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: CrateDigger.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    CrateDiggerWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
