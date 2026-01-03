defmodule CrateDigger.Collection.CrateTrack do
  use Ecto.Schema
  import Ecto.Changeset

  schema "crate_tracks" do
    field :position, :integer
    field :notes, :string
    field :crate_id, :id
    field :track_id, :id

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(crate_track, attrs) do
    crate_track
    |> cast(attrs, [:position, :notes])
    |> validate_required([:position, :notes])
  end
end
