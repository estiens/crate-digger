defmodule CrateDigger.Music.Track do
  use Ecto.Schema
  import Ecto.Changeset

  schema "tracks" do
    field :spotify_id, :string
    field :title, :string
    field :artist, :string
    field :album, :string
    field :image_url, :string
    field :preview_url, :string
    field :spotify_uri, :string
    field :bpm, :float
    field :key, :integer
    field :mode, :integer
    field :energy, :float
    field :danceability, :float
    field :valence, :float
    field :acousticness, :float
    field :instrumentalness, :float
    field :speechiness, :float
    field :liveness, :float
    field :loudness, :float
    field :time_signature, :integer
    field :popularity, :integer

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(track, attrs) do
    track
    |> cast(attrs, [:spotify_id, :title, :artist, :album, :image_url, :preview_url, :spotify_uri, :bpm, :key, :mode, :energy, :danceability, :valence, :acousticness, :instrumentalness, :speechiness, :liveness, :loudness, :time_signature, :popularity])
    |> validate_required([:spotify_id, :title, :artist, :album, :image_url, :preview_url, :spotify_uri, :bpm, :key, :mode, :energy, :danceability, :valence, :acousticness, :instrumentalness, :speechiness, :liveness, :loudness, :time_signature, :popularity])
    |> unique_constraint(:spotify_id)
  end
end
