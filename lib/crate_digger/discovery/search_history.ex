defmodule CrateDigger.Discovery.SearchHistory do
  use Ecto.Schema
  import Ecto.Changeset

  schema "search_history" do
    field :query, :string
    field :query_type, :string
    field :results_count, :integer
    field :user_id, :id
    field :seed_track_id, :id

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(search_history, attrs) do
    search_history
    |> cast(attrs, [:query, :query_type, :results_count])
    |> validate_required([:query, :query_type, :results_count])
  end
end
