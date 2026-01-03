defmodule CrateDigger.Repo.Migrations.CreateCrateTracks do
  use Ecto.Migration

  def change do
    create table(:crate_tracks) do
      add :position, :integer
      add :notes, :text
      add :crate_id, references(:crates, on_delete: :nothing)
      add :track_id, references(:tracks, on_delete: :nothing)

      timestamps(type: :utc_datetime)
    end

    create index(:crate_tracks, [:crate_id])
    create index(:crate_tracks, [:track_id])
  end
end
