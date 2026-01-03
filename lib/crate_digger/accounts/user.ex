defmodule CrateDigger.Accounts.User do
  use Ecto.Schema
  import Ecto.Changeset

  schema "users" do
    field :email, :string
    field :username, :string
    field :hashed_password, :string
    field :confirmed_at, :utc_datetime

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(user, attrs) do
    user
    |> cast(attrs, [:email, :username, :hashed_password, :confirmed_at])
    |> validate_required([:email, :username, :hashed_password, :confirmed_at])
    |> unique_constraint(:username)
    |> unique_constraint(:email)
  end
end
