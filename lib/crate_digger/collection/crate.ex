defmodule CrateDigger.Collection.Crate do
  use Ecto.Schema
  import Ecto.Changeset

  schema "crates" do
    field :name, :string
    field :description, :string
    field :is_public, :boolean, default: false
    field :user_id, :id

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(crate, attrs) do
    crate
    |> cast(attrs, [:name, :description, :is_public])
    |> validate_required([:name, :description, :is_public])
  end
end
