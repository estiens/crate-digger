defmodule CrateDigger.Clients.Spotify do
  @moduledoc """
  Spotify API client for track search, audio features, and recommendations.

  ## Configuration

  Set these environment variables:
  - SPOTIFY_CLIENT_ID
  - SPOTIFY_CLIENT_SECRET

  ## Features
  - Track search
  - Audio features (BPM, key, energy, danceability, etc.)
  - Recommendations based on seed tracks
  """

  require Logger

  @base_url "https://api.spotify.com/v1"
  @accounts_url "https://accounts.spotify.com/api/token"

  # Token cache (in production, use ETS or Redis)
  @token_agent __MODULE__.TokenAgent

  def start_link do
    Agent.start_link(fn -> %{token: nil, expires_at: nil} end, name: @token_agent)
  end

  @doc """
  Search for tracks by query string.
  """
  def search_tracks(query, opts \\ []) do
    limit = Keyword.get(opts, :limit, 10)

    with {:ok, token} <- get_access_token() do
      Req.get("#{@base_url}/search",
        headers: [{"Authorization", "Bearer #{token}"}],
        params: [q: query, type: "track", limit: limit]
      )
      |> handle_response(&parse_tracks/1)
    end
  end

  @doc """
  Get audio features for a track.
  Returns BPM, key, mode, energy, danceability, valence, etc.
  """
  def get_audio_features(track_id) do
    with {:ok, token} <- get_access_token() do
      Req.get("#{@base_url}/audio-features/#{track_id}",
        headers: [{"Authorization", "Bearer #{token}"}]
      )
      |> handle_response(&parse_audio_features/1)
    end
  end

  @doc """
  Get audio features for multiple tracks at once.
  """
  def get_audio_features_batch(track_ids) when is_list(track_ids) do
    with {:ok, token} <- get_access_token() do
      Req.get("#{@base_url}/audio-features",
        headers: [{"Authorization", "Bearer #{token}"}],
        params: [ids: Enum.join(track_ids, ",")]
      )
      |> handle_response(fn body ->
        body["audio_features"]
        |> Enum.reject(&is_nil/1)
        |> Enum.map(&parse_audio_features/1)
      end)
    end
  end

  @doc """
  Get track recommendations based on seed tracks and target audio features.

  ## Options
  - `:seed_tracks` - List of Spotify track IDs (max 5)
  - `:seed_artists` - List of Spotify artist IDs (max 5)
  - `:seed_genres` - List of genre names (max 5)
  - `:limit` - Number of recommendations (default 20, max 100)
  - `:target_*` / `:min_*` / `:max_*` - Audio feature targets
    - acousticness, danceability, energy, instrumentalness
    - liveness, loudness, popularity, speechiness, tempo, valence
  """
  def get_recommendations(opts) do
    with {:ok, token} <- get_access_token() do
      params = build_recommendation_params(opts)

      Req.get("#{@base_url}/recommendations",
        headers: [{"Authorization", "Bearer #{token}"}],
        params: params
      )
      |> handle_response(&parse_recommendations/1)
    end
  end

  @doc """
  Get detailed track information.
  """
  def get_track(track_id) do
    with {:ok, token} <- get_access_token() do
      Req.get("#{@base_url}/tracks/#{track_id}",
        headers: [{"Authorization", "Bearer #{token}"}]
      )
      |> handle_response(&parse_track/1)
    end
  end

  @doc """
  Get available genre seeds for recommendations.
  """
  def get_genre_seeds do
    with {:ok, token} <- get_access_token() do
      Req.get("#{@base_url}/recommendations/available-genre-seeds",
        headers: [{"Authorization", "Bearer #{token}"}]
      )
      |> handle_response(fn body -> body["genres"] end)
    end
  end

  # Private functions

  defp get_access_token do
    case get_cached_token() do
      {:ok, token} -> {:ok, token}
      :expired -> refresh_token()
    end
  end

  defp get_cached_token do
    try do
      case Agent.get(@token_agent, & &1) do
        %{token: token, expires_at: expires_at} when not is_nil(token) ->
          if DateTime.compare(expires_at, DateTime.utc_now()) == :gt do
            {:ok, token}
          else
            :expired
          end

        _ ->
          :expired
      end
    catch
      :exit, _ -> :expired
    end
  end

  defp refresh_token do
    client_id = System.get_env("SPOTIFY_CLIENT_ID")
    client_secret = System.get_env("SPOTIFY_CLIENT_SECRET")

    if is_nil(client_id) or is_nil(client_secret) do
      {:error, :missing_credentials}
    else
      credentials = Base.encode64("#{client_id}:#{client_secret}")

      case Req.post(@accounts_url,
             headers: [
               {"Authorization", "Basic #{credentials}"},
               {"Content-Type", "application/x-www-form-urlencoded"}
             ],
             body: "grant_type=client_credentials"
           ) do
        {:ok, %{status: 200, body: body}} ->
          token = body["access_token"]
          expires_in = body["expires_in"] || 3600
          expires_at = DateTime.add(DateTime.utc_now(), expires_in - 60, :second)

          try do
            Agent.update(@token_agent, fn _ -> %{token: token, expires_at: expires_at} end)
          catch
            :exit, _ -> :ok
          end

          {:ok, token}

        {:ok, %{status: status, body: body}} ->
          Logger.error("Spotify auth failed: #{status} - #{inspect(body)}")
          {:error, :auth_failed}

        {:error, reason} ->
          Logger.error("Spotify auth error: #{inspect(reason)}")
          {:error, reason}
      end
    end
  end

  defp handle_response({:ok, %{status: 200, body: body}}, parser) do
    {:ok, parser.(body)}
  end

  defp handle_response({:ok, %{status: 401}}, _parser) do
    # Token expired, try to refresh
    {:error, :unauthorized}
  end

  defp handle_response({:ok, %{status: 429, headers: headers}}, _parser) do
    retry_after = get_header(headers, "retry-after") || "60"
    {:error, {:rate_limited, String.to_integer(retry_after)}}
  end

  defp handle_response({:ok, %{status: status, body: body}}, _parser) do
    Logger.error("Spotify API error: #{status} - #{inspect(body)}")
    {:error, {:api_error, status, body}}
  end

  defp handle_response({:error, reason}, _parser) do
    {:error, reason}
  end

  defp get_header(headers, name) do
    headers
    |> Enum.find(fn {k, _v} -> String.downcase(k) == name end)
    |> case do
      {_, value} -> value
      nil -> nil
    end
  end

  defp parse_tracks(%{"tracks" => %{"items" => items}}) do
    Enum.map(items, &parse_track/1)
  end

  defp parse_track(item) do
    %{
      spotify_id: item["id"],
      title: item["name"],
      artist: item["artists"] |> Enum.map(& &1["name"]) |> Enum.join(", "),
      artist_ids: item["artists"] |> Enum.map(& &1["id"]),
      album: get_in(item, ["album", "name"]),
      image_url: get_in(item, ["album", "images"]) |> List.first() |> get_image_url(),
      preview_url: item["preview_url"],
      spotify_uri: item["uri"],
      popularity: item["popularity"],
      duration_ms: item["duration_ms"]
    }
  end

  defp get_image_url(nil), do: nil
  defp get_image_url(image), do: image["url"]

  defp parse_audio_features(features) do
    %{
      spotify_id: features["id"],
      bpm: features["tempo"],
      key: features["key"],
      mode: features["mode"],
      energy: features["energy"],
      danceability: features["danceability"],
      valence: features["valence"],
      acousticness: features["acousticness"],
      instrumentalness: features["instrumentalness"],
      speechiness: features["speechiness"],
      liveness: features["liveness"],
      loudness: features["loudness"],
      time_signature: features["time_signature"],
      duration_ms: features["duration_ms"]
    }
  end

  defp parse_recommendations(%{"tracks" => tracks}) do
    Enum.map(tracks, &parse_track/1)
  end

  defp build_recommendation_params(opts) do
    base_params = [
      seed_tracks: opts[:seed_tracks] |> List.wrap() |> Enum.join(","),
      seed_artists: opts[:seed_artists] |> List.wrap() |> Enum.join(","),
      seed_genres: opts[:seed_genres] |> List.wrap() |> Enum.join(","),
      limit: opts[:limit] || 20
    ]

    # Audio feature targets
    feature_params =
      ~w(acousticness danceability energy instrumentalness liveness loudness popularity speechiness tempo valence)
      |> Enum.flat_map(fn feature ->
        [
          {String.to_atom("target_#{feature}"), opts[String.to_atom("target_#{feature}")]},
          {String.to_atom("min_#{feature}"), opts[String.to_atom("min_#{feature}")]},
          {String.to_atom("max_#{feature}"), opts[String.to_atom("max_#{feature}")]}
        ]
      end)

    (base_params ++ feature_params)
    |> Enum.reject(fn {_k, v} -> is_nil(v) or v == "" end)
  end
end
