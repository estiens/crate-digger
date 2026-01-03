defmodule CrateDiggerWeb.DiscoverLive do
  use CrateDiggerWeb, :live_view

  alias CrateDigger.Clients.Spotify
  alias CrateDigger.Discovery.LLM
  alias CrateDigger.Affiliate.Links

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(:query, "")
     |> assign(:search_mode, :text)
     |> assign(:tracks, [])
     |> assign(:recommendations, [])
     |> assign(:selected_track, nil)
     |> assign(:audio_features, nil)
     |> assign(:track_description, nil)
     |> assign(:crate, [])
     |> assign(:show_crate, false)
     |> assign(:loading, false)
     |> assign(:error, nil)
     |> assign(:filters, default_filters())
     |> assign(:platforms, Links.platform_info())}
  end

  @impl true
  def handle_event("search", %{"query" => query}, socket) do
    socket = assign(socket, loading: true, error: nil)
    send(self(), {:do_search, query})
    {:noreply, socket}
  end

  @impl true
  def handle_event("natural_search", %{"query" => query}, socket) do
    socket = assign(socket, loading: true, error: nil, search_mode: :natural)
    send(self(), {:do_natural_search, query})
    {:noreply, socket}
  end

  @impl true
  def handle_event("select_track", %{"id" => spotify_id}, socket) do
    track = Enum.find(socket.assigns.tracks, &(&1.spotify_id == spotify_id))

    if track do
      socket = assign(socket, selected_track: track, loading: true)
      send(self(), {:load_features, spotify_id})
      {:noreply, socket}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("get_recommendations", _params, socket) do
    if socket.assigns.selected_track do
      socket = assign(socket, :loading, true)
      send(self(), :do_recommendations)
      {:noreply, socket}
    else
      {:noreply, put_flash(socket, :error, "Select a track first")}
    end
  end

  @impl true
  def handle_event("update_filter", %{"filter" => filter, "value" => value}, socket) do
    {value, _} = Float.parse(value)
    filters = Map.put(socket.assigns.filters, String.to_atom(filter), value)
    {:noreply, assign(socket, :filters, filters)}
  end

  @impl true
  def handle_event("add_to_crate", %{"id" => spotify_id}, socket) do
    track =
      Enum.find(socket.assigns.tracks ++ socket.assigns.recommendations, &(&1.spotify_id == spotify_id))

    if track && not Enum.any?(socket.assigns.crate, &(&1.spotify_id == spotify_id)) do
      crate = socket.assigns.crate ++ [track]
      {:noreply, socket |> assign(:crate, crate) |> put_flash(:info, "Added to crate!")}
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_event("remove_from_crate", %{"id" => spotify_id}, socket) do
    crate = Enum.reject(socket.assigns.crate, &(&1.spotify_id == spotify_id))
    {:noreply, assign(socket, :crate, crate)}
  end

  @impl true
  def handle_event("toggle_crate", _params, socket) do
    {:noreply, assign(socket, :show_crate, not socket.assigns.show_crate)}
  end

  @impl true
  def handle_event("clear_crate", _params, socket) do
    {:noreply, assign(socket, :crate, [])}
  end

  @impl true
  def handle_info({:do_search, query}, socket) do
    case Spotify.search_tracks(query, limit: 8) do
      {:ok, tracks} ->
        {:noreply, assign(socket, tracks: tracks, loading: false, query: query)}

      {:error, reason} ->
        {:noreply, assign(socket, loading: false, error: format_error(reason))}
    end
  end

  @impl true
  def handle_info({:do_natural_search, query}, socket) do
    with {:ok, params} <- LLM.parse_music_request(query),
         {:ok, tracks} <- Spotify.get_recommendations(params) do
      {:noreply, assign(socket, recommendations: tracks, loading: false, query: query)}
    else
      {:error, reason} ->
        {:noreply, assign(socket, loading: false, error: format_error(reason))}
    end
  end

  @impl true
  def handle_info({:load_features, spotify_id}, socket) do
    case Spotify.get_audio_features(spotify_id) do
      {:ok, features} ->
        socket = assign(socket, audio_features: features, loading: false)

        # Get AI description in background
        if System.get_env("ANTHROPIC_API_KEY") || System.get_env("OPENAI_API_KEY") do
          send(self(), :generate_description)
        end

        {:noreply, socket}

      {:error, reason} ->
        {:noreply, assign(socket, loading: false, error: format_error(reason))}
    end
  end

  @impl true
  def handle_info(:generate_description, socket) do
    track = socket.assigns.selected_track
    features = socket.assigns.audio_features

    if track && features do
      track_with_features = Map.merge(track, features)

      case LLM.describe_track(track_with_features) do
        {:ok, description} ->
          {:noreply, assign(socket, :track_description, description)}

        {:error, _} ->
          {:noreply, socket}
      end
    else
      {:noreply, socket}
    end
  end

  @impl true
  def handle_info(:do_recommendations, socket) do
    track = socket.assigns.selected_track
    filters = socket.assigns.filters

    params =
      filters
      |> Map.put(:seed_tracks, [track.spotify_id])
      |> Map.put(:limit, 12)

    case Spotify.get_recommendations(params) do
      {:ok, tracks} ->
        {:noreply, assign(socket, recommendations: tracks, loading: false)}

      {:error, reason} ->
        {:noreply, assign(socket, loading: false, error: format_error(reason))}
    end
  end

  # Private functions

  defp default_filters do
    %{
      target_energy: 0.5,
      target_danceability: 0.5,
      target_valence: 0.5,
      min_tempo: 100,
      max_tempo: 140
    }
  end

  defp format_error(:missing_credentials), do: "Spotify API credentials not configured"
  defp format_error(:unauthorized), do: "Spotify API authentication failed"
  defp format_error({:rate_limited, seconds}), do: "Rate limited. Try again in #{seconds}s"
  defp format_error(:no_api_key), do: "LLM API key not configured"
  defp format_error(reason), do: "Error: #{inspect(reason)}"

  @impl true
  def render(assigns) do
    ~H"""
    <div class="min-h-screen bg-base-200">
      <!-- Header -->
      <header class="navbar bg-base-100 shadow-lg">
        <div class="flex-1">
          <a class="btn btn-ghost text-xl font-bold">
            <span class="text-primary">CR8</span>digger
          </a>
        </div>
        <div class="flex-none gap-2">
          <button class="btn btn-ghost btn-circle" phx-click="toggle_crate">
            <div class="indicator">
              <.icon name="hero-heart" class="h-6 w-6" />
              <span :if={length(@crate) > 0} class="badge badge-sm badge-primary indicator-item">
                {length(@crate)}
              </span>
            </div>
          </button>
        </div>
      </header>

      <main class="container mx-auto px-4 py-8">
        <!-- Search Section -->
        <div class="card bg-base-100 shadow-xl mb-8">
          <div class="card-body">
            <h2 class="card-title mb-4">Find Your Sound</h2>

            <!-- Search Tabs -->
            <div role="tablist" class="tabs tabs-boxed mb-4">
              <a
                role="tab"
                class={"tab #{if @search_mode == :text, do: "tab-active"}"}
                phx-click="search_mode"
                phx-value-mode="text"
              >
                Track Search
              </a>
              <a
                role="tab"
                class={"tab #{if @search_mode == :natural, do: "tab-active"}"}
                phx-click="search_mode"
                phx-value-mode="natural"
              >
                Describe It
              </a>
            </div>

            <!-- Text Search -->
            <form :if={@search_mode == :text} phx-submit="search" class="flex gap-2">
              <input
                type="text"
                name="query"
                value={@query}
                placeholder="Search for artist or track..."
                class="input input-bordered flex-1"
              />
              <button type="submit" class="btn btn-primary" disabled={@loading}>
                <span :if={@loading} class="loading loading-spinner loading-sm"></span>
                Search
              </button>
            </form>

            <!-- Natural Language Search -->
            <form :if={@search_mode == :natural} phx-submit="natural_search" class="flex gap-2">
              <input
                type="text"
                name="query"
                value={@query}
                placeholder="e.g., dark minimal techno around 125 BPM with analog warmth..."
                class="input input-bordered flex-1"
              />
              <button type="submit" class="btn btn-secondary" disabled={@loading}>
                <span :if={@loading} class="loading loading-spinner loading-sm"></span>
                Discover
              </button>
            </form>

            <!-- Error Display -->
            <div :if={@error} class="alert alert-error mt-4">
              <.icon name="hero-exclamation-circle" class="h-6 w-6" />
              <span>{@error}</span>
            </div>
          </div>
        </div>

        <!-- Search Results -->
        <div :if={length(@tracks) > 0} class="mb-8">
          <h3 class="text-xl font-bold mb-4">Search Results</h3>
          <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
            <%= for track <- @tracks do %>
              <.track_card
                track={track}
                selected={@selected_track && @selected_track.spotify_id == track.spotify_id}
                in_crate={Enum.any?(@crate, &(&1.spotify_id == track.spotify_id))}
              />
            <% end %>
          </div>
        </div>

        <!-- Selected Track Details -->
        <div :if={@selected_track} class="card bg-base-100 shadow-xl mb-8">
          <div class="card-body">
            <div class="flex flex-col lg:flex-row gap-6">
              <!-- Track Info -->
              <div class="flex-1">
                <h2 class="card-title text-2xl">{@selected_track.title}</h2>
                <p class="text-lg opacity-70">{@selected_track.artist}</p>
                <p class="text-sm opacity-50">{@selected_track.album}</p>

                <!-- AI Description -->
                <p :if={@track_description} class="mt-4 italic text-base-content/80">
                  "{@track_description}"
                </p>

                <!-- Audio Preview -->
                <div :if={@selected_track.preview_url} class="mt-4">
                  <audio controls class="w-full">
                    <source src={@selected_track.preview_url} type="audio/mpeg" />
                  </audio>
                </div>

                <!-- Buy Links -->
                <div class="mt-4">
                  <h4 class="font-bold mb-2">Buy / Stream</h4>
                  <div class="flex flex-wrap gap-2">
                    <%= for platform <- @platforms do %>
                      <a
                        href={get_platform_link(@selected_track, platform.id)}
                        target="_blank"
                        class="btn btn-sm"
                        style={"background-color: #{platform.color}; color: white;"}
                      >
                        {platform.name}
                        <span :if={platform.commission != "none"} class="badge badge-xs">
                          {platform.commission}
                        </span>
                      </a>
                    <% end %>
                  </div>
                </div>
              </div>

              <!-- Audio Features -->
              <div :if={@audio_features} class="flex-1">
                <h3 class="font-bold mb-4">Audio Features</h3>
                <div class="space-y-2">
                  <.feature_bar label="Energy" value={@audio_features.energy} />
                  <.feature_bar label="Danceability" value={@audio_features.danceability} />
                  <.feature_bar label="Valence" value={@audio_features.valence} color="warning" />
                  <.feature_bar
                    label="Acousticness"
                    value={@audio_features.acousticness}
                    color="info"
                  />
                  <.feature_bar
                    label="Instrumentalness"
                    value={@audio_features.instrumentalness}
                    color="secondary"
                  />
                </div>
                <div class="mt-4 grid grid-cols-2 gap-2 text-sm">
                  <div class="stat bg-base-200 rounded-lg p-2">
                    <div class="stat-title text-xs">BPM</div>
                    <div class="stat-value text-lg">{round(@audio_features.bpm)}</div>
                  </div>
                  <div class="stat bg-base-200 rounded-lg p-2">
                    <div class="stat-title text-xs">Key</div>
                    <div class="stat-value text-lg">{format_key(@audio_features.key, @audio_features.mode)}</div>
                  </div>
                </div>
              </div>
            </div>

            <!-- Get Recommendations Button -->
            <div class="card-actions justify-end mt-4">
              <button class="btn btn-primary" phx-click="get_recommendations" disabled={@loading}>
                <span :if={@loading} class="loading loading-spinner loading-sm"></span>
                Find Similar Tracks
              </button>
            </div>
          </div>
        </div>

        <!-- Filter Sliders -->
        <div :if={@selected_track} class="card bg-base-100 shadow-xl mb-8">
          <div class="card-body">
            <h3 class="card-title">Tune Your Recommendations</h3>
            <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
              <.filter_slider
                name="target_energy"
                label="Energy"
                value={@filters.target_energy}
                min={0}
                max={1}
                step={0.1}
              />
              <.filter_slider
                name="target_danceability"
                label="Danceability"
                value={@filters.target_danceability}
                min={0}
                max={1}
                step={0.1}
              />
              <.filter_slider
                name="target_valence"
                label="Mood (Dark → Bright)"
                value={@filters.target_valence}
                min={0}
                max={1}
                step={0.1}
              />
              <.filter_slider
                name="min_tempo"
                label="Min BPM"
                value={@filters.min_tempo}
                min={60}
                max={200}
                step={5}
              />
              <.filter_slider
                name="max_tempo"
                label="Max BPM"
                value={@filters.max_tempo}
                min={60}
                max={200}
                step={5}
              />
            </div>
          </div>
        </div>

        <!-- Recommendations -->
        <div :if={length(@recommendations) > 0} class="mb-8">
          <h3 class="text-xl font-bold mb-4">Recommended Tracks</h3>
          <div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
            <%= for track <- @recommendations do %>
              <.track_card
                track={track}
                selected={false}
                in_crate={Enum.any?(@crate, &(&1.spotify_id == track.spotify_id))}
              />
            <% end %>
          </div>
        </div>

        <!-- Crate Drawer -->
        <div :if={@show_crate} class="fixed inset-y-0 right-0 w-80 bg-base-100 shadow-xl z-50 p-4">
          <div class="flex justify-between items-center mb-4">
            <h3 class="text-xl font-bold">Your Crate ({length(@crate)})</h3>
            <button class="btn btn-ghost btn-sm" phx-click="toggle_crate">✕</button>
          </div>

          <div :if={length(@crate) == 0} class="text-center opacity-50 py-8">
            Your crate is empty. Click the heart on tracks to add them!
          </div>

          <div class="space-y-2">
            <%= for track <- @crate do %>
              <div class="flex items-center gap-2 p-2 bg-base-200 rounded-lg">
                <img src={track.image_url} class="w-12 h-12 rounded" />
                <div class="flex-1 min-w-0">
                  <p class="font-bold truncate text-sm">{track.title}</p>
                  <p class="text-xs opacity-70 truncate">{track.artist}</p>
                </div>
                <button class="btn btn-ghost btn-xs" phx-click="remove_from_crate" phx-value-id={track.spotify_id}>
                  ✕
                </button>
              </div>
            <% end %>
          </div>

          <div :if={length(@crate) > 0} class="mt-4">
            <button class="btn btn-error btn-sm w-full" phx-click="clear_crate">
              Clear Crate
            </button>
          </div>
        </div>
      </main>
    </div>
    """
  end

  # Components

  defp track_card(assigns) do
    ~H"""
    <div class={"card bg-base-100 shadow-md hover:shadow-xl transition-shadow cursor-pointer #{if @selected, do: "ring-2 ring-primary"}"}>
      <figure class="relative">
        <img src={@track.image_url || "/images/placeholder.png"} alt={@track.title} class="w-full aspect-square object-cover" />
        <div class="absolute inset-0 bg-black/0 hover:bg-black/20 transition-colors flex items-center justify-center">
          <button
            :if={@track.preview_url}
            class="btn btn-circle btn-primary opacity-0 hover:opacity-100"
          >
            ▶
          </button>
        </div>
      </figure>
      <div class="card-body p-4">
        <h3 class="card-title text-sm truncate" phx-click="select_track" phx-value-id={@track.spotify_id}>
          {@track.title}
        </h3>
        <p class="text-xs opacity-70 truncate">{@track.artist}</p>
        <div class="card-actions justify-end mt-2">
          <button
            class={"btn btn-sm #{if @in_crate, do: "btn-primary", else: "btn-ghost"}"}
            phx-click="add_to_crate"
            phx-value-id={@track.spotify_id}
          >
            <.icon name={if @in_crate, do: "hero-heart-solid", else: "hero-heart"} class="h-4 w-4" />
          </button>
        </div>
      </div>
    </div>
    """
  end

  defp feature_bar(assigns) do
    assigns = assign_new(assigns, :color, fn -> "primary" end)

    ~H"""
    <div>
      <div class="flex justify-between text-sm mb-1">
        <span>{@label}</span>
        <span>{round(@value * 100)}%</span>
      </div>
      <progress class={"progress progress-#{@color}"} value={@value * 100} max="100"></progress>
    </div>
    """
  end

  defp filter_slider(assigns) do
    ~H"""
    <div class="form-control">
      <label class="label">
        <span class="label-text">{@label}</span>
        <span class="label-text-alt">{@value}</span>
      </label>
      <input
        type="range"
        min={@min}
        max={@max}
        step={@step}
        value={@value}
        class="range range-primary range-sm"
        phx-change="update_filter"
        phx-value-filter={@name}
        name="value"
      />
    </div>
    """
  end

  defp format_key(nil, _), do: "?"

  defp format_key(key, mode) do
    keys = ~w(C C# D D# E F F# G G# A A# B)
    mode_str = if mode == 1, do: "", else: "m"
    "#{Enum.at(keys, key)}#{mode_str}"
  end

  defp get_platform_link(track, platform) do
    case platform do
      :beatport -> Links.generate_beatport_link(track)
      :traxsource -> Links.generate_traxsource_link(track)
      :juno -> Links.generate_juno_link(track)
      :apple_music -> Links.generate_apple_music_link(track)
      :amazon -> Links.generate_amazon_link(track)
      :spotify -> Links.generate_spotify_link(track)
      _ -> "#"
    end
  end
end
