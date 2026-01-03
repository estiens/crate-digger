defmodule CrateDigger.Repo.Migrations.CreateSearchHistory do
  use Ecto.Migration

  def change do
    create table(:search_history) do
      add :query, :string
      add :query_type, :string
      add :results_count, :integer
      add :user_id, references(:users, on_delete: :nothing)
      add :seed_track_id, references(:tracks, on_delete: :nothing)

      timestamps(type: :utc_datetime)
    end

    create index(:search_history, [:user_id])
    create index(:search_history, [:seed_track_id])
  end
end
