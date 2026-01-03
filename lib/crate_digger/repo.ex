defmodule CrateDigger.Repo do
  use Ecto.Repo,
    otp_app: :crate_digger,
    adapter: Ecto.Adapters.Postgres
end
