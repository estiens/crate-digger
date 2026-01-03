defmodule CrateDigger.Repo.Migrations.CreateTracks do
  use Ecto.Migration

  def change do
    create table(:tracks) do
      add :spotify_id, :string
      add :title, :string
      add :artist, :string
      add :album, :string
      add :image_url, :string
      add :preview_url, :string
      add :spotify_uri, :string
      add :bpm, :float
      add :key, :integer
      add :mode, :integer
      add :energy, :float
      add :danceability, :float
      add :valence, :float
      add :acousticness, :float
      add :instrumentalness, :float
      add :speechiness, :float
      add :liveness, :float
      add :loudness, :float
      add :time_signature, :integer
      add :popularity, :integer

      timestamps(type: :utc_datetime)
    end

    create unique_index(:tracks, [:spotify_id])
  end
end
