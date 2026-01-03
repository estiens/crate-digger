defmodule CrateDigger.Discovery.LLM do
  @moduledoc """
  LLM-powered music discovery using natural language.

  Supports:
  - Natural language to audio feature translation
  - Track description generation
  - Genre/mood interpretation

  ## Configuration

  Set one of these environment variables:
  - ANTHROPIC_API_KEY - for Claude
  - OPENAI_API_KEY - for GPT-4
  """

  require Logger

  @anthropic_url "https://api.anthropic.com/v1/messages"
  @openai_url "https://api.openai.com/v1/chat/completions"

  @doc """
  Parse a natural language music request into Spotify recommendation parameters.

  ## Examples

      iex> parse_music_request("dark minimal techno with analog warmth around 125 BPM")
      {:ok, %{
        seed_genres: ["minimal-techno", "techno"],
        target_energy: 0.7,
        target_valence: 0.3,
        min_tempo: 120,
        max_tempo: 130,
        target_acousticness: 0.2,
        target_instrumentalness: 0.9
      }}
  """
  def parse_music_request(text) do
    prompt = """
    You are a music recommendation expert. Parse the following music request into Spotify API recommendation parameters.

    Available parameters (all values 0.0-1.0 unless noted):
    - seed_genres: list of Spotify genre seeds (e.g., "techno", "house", "ambient", "drum-and-bass")
    - target_energy: intensity and activity (0=calm, 1=energetic)
    - target_valence: musical positivity (0=sad/dark, 1=happy/cheerful)
    - target_danceability: how suitable for dancing
    - target_acousticness: presence of acoustic instruments
    - target_instrumentalness: lack of vocals (1=instrumental)
    - target_speechiness: presence of spoken words
    - target_liveness: presence of live audience
    - min_tempo / max_tempo / target_tempo: BPM (typical range 60-200)
    - min_popularity / max_popularity: 0-100

    User request: "#{text}"

    Respond ONLY with valid JSON. No explanation. Example:
    {"seed_genres": ["techno"], "target_energy": 0.8, "min_tempo": 125, "max_tempo": 135}
    """

    case call_llm(prompt) do
      {:ok, response} ->
        parse_json_response(response)

      {:error, reason} ->
        Logger.error("LLM parse error: #{inspect(reason)}")
        {:error, reason}
    end
  end

  @doc """
  Generate a natural language description of a track based on its audio features.
  """
  def describe_track(track) do
    prompt = """
    You are a music journalist. Describe this track in 2-3 evocative sentences for DJs.
    Focus on: energy, mood, danceability, and what kind of set it would fit in.

    Track: "#{track.title}" by #{track.artist}
    Audio features:
    - BPM: #{track.bpm}
    - Energy: #{format_percent(track.energy)}
    - Danceability: #{format_percent(track.danceability)}
    - Valence (mood): #{format_percent(track.valence)} (0%=dark, 100%=bright)
    - Acousticness: #{format_percent(track.acousticness)}
    - Instrumentalness: #{format_percent(track.instrumentalness)}
    - Key: #{format_key(track.key, track.mode)}

    Be concise and use DJ/producer terminology. No bullet points.
    """

    case call_llm(prompt) do
      {:ok, response} -> {:ok, String.trim(response)}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Suggest similar artists or genres based on a description.
  """
  def suggest_similar(description) do
    prompt = """
    Based on this music description, suggest 5 similar artists and 3 Spotify genre seeds.

    Description: "#{description}"

    Respond ONLY with JSON:
    {"artists": ["Artist 1", "Artist 2", ...], "genres": ["genre-1", "genre-2", "genre-3"]}
    """

    case call_llm(prompt) do
      {:ok, response} -> parse_json_response(response)
      {:error, reason} -> {:error, reason}
    end
  end

  @doc """
  Interpret mood/vibe keywords into audio feature ranges.
  """
  def interpret_vibe(vibe) do
    prompt = """
    Convert this music vibe/mood description into Spotify audio feature ranges.

    Vibe: "#{vibe}"

    Respond ONLY with JSON containing min/max or target values:
    {
      "energy": {"min": 0.0, "max": 1.0},
      "valence": {"min": 0.0, "max": 1.0},
      "danceability": {"target": 0.5},
      "tempo": {"min": 100, "max": 130}
    }
    """

    case call_llm(prompt) do
      {:ok, response} -> parse_json_response(response)
      {:error, reason} -> {:error, reason}
    end
  end

  # Private functions

  defp call_llm(prompt) do
    cond do
      anthropic_key = System.get_env("ANTHROPIC_API_KEY") ->
        call_anthropic(prompt, anthropic_key)

      openai_key = System.get_env("OPENAI_API_KEY") ->
        call_openai(prompt, openai_key)

      true ->
        {:error, :no_api_key}
    end
  end

  defp call_anthropic(prompt, api_key) do
    body = %{
      model: "claude-sonnet-4-20250514",
      max_tokens: 1024,
      messages: [%{role: "user", content: prompt}]
    }

    case Req.post(@anthropic_url,
           headers: [
             {"x-api-key", api_key},
             {"anthropic-version", "2023-06-01"},
             {"content-type", "application/json"}
           ],
           json: body
         ) do
      {:ok, %{status: 200, body: %{"content" => [%{"text" => text} | _]}}} ->
        {:ok, text}

      {:ok, %{status: status, body: body}} ->
        {:error, {:api_error, status, body}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp call_openai(prompt, api_key) do
    body = %{
      model: "gpt-4o-mini",
      messages: [%{role: "user", content: prompt}],
      max_tokens: 1024
    }

    case Req.post(@openai_url,
           headers: [
             {"Authorization", "Bearer #{api_key}"},
             {"content-type", "application/json"}
           ],
           json: body
         ) do
      {:ok, %{status: 200, body: %{"choices" => [%{"message" => %{"content" => text}} | _]}}} ->
        {:ok, text}

      {:ok, %{status: status, body: body}} ->
        {:error, {:api_error, status, body}}

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp parse_json_response(text) do
    # Extract JSON from response (in case there's extra text)
    json_pattern = ~r/\{[^{}]*\}/s

    case Regex.run(json_pattern, text) do
      [json_str] ->
        case Jason.decode(json_str) do
          {:ok, parsed} -> {:ok, atomize_keys(parsed)}
          {:error, _} -> {:error, :invalid_json}
        end

      nil ->
        {:error, :no_json_found}
    end
  end

  defp atomize_keys(map) when is_map(map) do
    Map.new(map, fn {k, v} ->
      key = if is_binary(k), do: String.to_atom(k), else: k
      {key, atomize_keys(v)}
    end)
  end

  defp atomize_keys(list) when is_list(list), do: Enum.map(list, &atomize_keys/1)
  defp atomize_keys(value), do: value

  defp format_percent(nil), do: "N/A"
  defp format_percent(value) when is_float(value), do: "#{round(value * 100)}%"

  defp format_key(nil, _), do: "Unknown"

  defp format_key(key, mode) do
    keys = ~w(C C#/Db D D#/Eb E F F#/Gb G G#/Ab A A#/Bb B)
    mode_str = if mode == 1, do: "Major", else: "Minor"
    "#{Enum.at(keys, key)} #{mode_str}"
  end
end
