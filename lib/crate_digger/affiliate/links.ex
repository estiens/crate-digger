defmodule CrateDigger.Affiliate.Links do
  @moduledoc """
  Generates affiliate links for various music platforms.

  ## Supported Platforms
  - Beatport (5-10% commission)
  - Traxsource (via Yeesshh)
  - Juno Download (5-10% commission)
  - Apple Music / iTunes (7% commission)
  - Amazon Music (5% commission)

  ## Configuration

  Set these environment variables:
  - BEATPORT_AFFILIATE_ID
  - TRAXSOURCE_AFFILIATE_ID
  - JUNO_AFFILIATE_ID
  - APPLE_AFFILIATE_TOKEN
  - AMAZON_ASSOCIATE_TAG
  """

  alias CrateDigger.Affiliate.Click
  alias CrateDigger.Repo

  @doc """
  Generate affiliate links for all supported platforms for a given track.
  """
  def generate_all_links(track) do
    %{
      beatport: generate_beatport_link(track),
      traxsource: generate_traxsource_link(track),
      juno: generate_juno_link(track),
      apple_music: generate_apple_music_link(track),
      amazon: generate_amazon_link(track),
      spotify: generate_spotify_link(track)
    }
    |> Enum.reject(fn {_k, v} -> is_nil(v) end)
    |> Map.new()
  end

  @doc """
  Generate a trackable affiliate link with click tracking.
  Returns a local redirect URL that tracks the click before redirecting.
  """
  def generate_tracked_link(track, platform, user_id \\ nil) do
    # Generate a unique click token
    click_token = generate_click_token()

    # The actual affiliate link
    affiliate_link = get_affiliate_link(track, platform)

    if affiliate_link do
      # Store pending click in database
      %Click{}
      |> Click.changeset(%{
        track_id: track.id,
        user_id: user_id,
        platform: to_string(platform),
        affiliate_link: affiliate_link,
        clicked_at: nil,
        converted: false
      })
      |> Repo.insert()

      # Return local tracking URL
      "/go/#{platform}/#{click_token}"
    else
      nil
    end
  end

  @doc """
  Generate Beatport search link with affiliate tracking.
  """
  def generate_beatport_link(track) do
    affiliate_id = System.get_env("BEATPORT_AFFILIATE_ID")

    if affiliate_id do
      query = URI.encode("#{track.artist} #{track.title}")
      "https://www.beatport.com/search?q=#{query}&aff_id=#{affiliate_id}"
    else
      # Fallback without affiliate
      query = URI.encode("#{track.artist} #{track.title}")
      "https://www.beatport.com/search?q=#{query}"
    end
  end

  @doc """
  Generate Traxsource search link with affiliate tracking.
  """
  def generate_traxsource_link(track) do
    affiliate_id = System.get_env("TRAXSOURCE_AFFILIATE_ID")

    query = URI.encode("#{track.artist} #{track.title}")
    base_url = "https://www.traxsource.com/search?term=#{query}"

    if affiliate_id do
      "#{base_url}&aff=#{affiliate_id}"
    else
      base_url
    end
  end

  @doc """
  Generate Juno Download search link with affiliate tracking.
  """
  def generate_juno_link(track) do
    affiliate_id = System.get_env("JUNO_AFFILIATE_ID")

    query = URI.encode("#{track.artist} #{track.title}")
    base_url = "https://www.junodownload.com/search/?q[all][]=#{query}"

    if affiliate_id do
      "#{base_url}&ref=#{affiliate_id}"
    else
      base_url
    end
  end

  @doc """
  Generate Apple Music link with affiliate token.
  Uses the iTunes Search API format.
  """
  def generate_apple_music_link(track) do
    affiliate_token = System.get_env("APPLE_AFFILIATE_TOKEN")

    query = URI.encode("#{track.artist} #{track.title}")
    base_url = "https://music.apple.com/search?term=#{query}"

    if affiliate_token do
      "#{base_url}&at=#{affiliate_token}"
    else
      base_url
    end
  end

  @doc """
  Generate Amazon Music search link with associate tag.
  """
  def generate_amazon_link(track) do
    associate_tag = System.get_env("AMAZON_ASSOCIATE_TAG")

    query = URI.encode("#{track.artist} #{track.title}")
    base_url = "https://www.amazon.com/s?k=#{query}&i=digital-music"

    if associate_tag do
      "#{base_url}&tag=#{associate_tag}"
    else
      base_url
    end
  end

  @doc """
  Generate Spotify direct link (no affiliate, but useful for streaming).
  """
  def generate_spotify_link(track) do
    if track.spotify_uri do
      # Deep link for Spotify app
      track.spotify_uri
    else
      # Web fallback
      "https://open.spotify.com/search/#{URI.encode("#{track.artist} #{track.title}")}"
    end
  end

  @doc """
  Get platform display info for UI.
  """
  def platform_info do
    [
      %{
        id: :beatport,
        name: "Beatport",
        description: "Electronic music store",
        commission: "5-10%",
        icon: "beatport",
        color: "#94d500",
        has_affiliate: has_affiliate?(:beatport)
      },
      %{
        id: :traxsource,
        name: "Traxsource",
        description: "House & Techno",
        commission: "varies",
        icon: "traxsource",
        color: "#00a7e1",
        has_affiliate: has_affiliate?(:traxsource)
      },
      %{
        id: :juno,
        name: "Juno Download",
        description: "Electronic music",
        commission: "5-10%",
        icon: "juno",
        color: "#ff6600",
        has_affiliate: has_affiliate?(:juno)
      },
      %{
        id: :apple_music,
        name: "Apple Music",
        description: "Stream or buy",
        commission: "7%",
        icon: "apple",
        color: "#fa243c",
        has_affiliate: has_affiliate?(:apple_music)
      },
      %{
        id: :amazon,
        name: "Amazon Music",
        description: "Stream or buy",
        commission: "5%",
        icon: "amazon",
        color: "#ff9900",
        has_affiliate: has_affiliate?(:amazon)
      },
      %{
        id: :spotify,
        name: "Spotify",
        description: "Stream only",
        commission: "none",
        icon: "spotify",
        color: "#1db954",
        has_affiliate: false
      }
    ]
  end

  # Private functions

  defp get_affiliate_link(track, :beatport), do: generate_beatport_link(track)
  defp get_affiliate_link(track, :traxsource), do: generate_traxsource_link(track)
  defp get_affiliate_link(track, :juno), do: generate_juno_link(track)
  defp get_affiliate_link(track, :apple_music), do: generate_apple_music_link(track)
  defp get_affiliate_link(track, :amazon), do: generate_amazon_link(track)
  defp get_affiliate_link(track, :spotify), do: generate_spotify_link(track)
  defp get_affiliate_link(_track, _platform), do: nil

  defp has_affiliate?(:beatport), do: not is_nil(System.get_env("BEATPORT_AFFILIATE_ID"))
  defp has_affiliate?(:traxsource), do: not is_nil(System.get_env("TRAXSOURCE_AFFILIATE_ID"))
  defp has_affiliate?(:juno), do: not is_nil(System.get_env("JUNO_AFFILIATE_ID"))
  defp has_affiliate?(:apple_music), do: not is_nil(System.get_env("APPLE_AFFILIATE_TOKEN"))
  defp has_affiliate?(:amazon), do: not is_nil(System.get_env("AMAZON_ASSOCIATE_TAG"))
  defp has_affiliate?(_), do: false

  defp generate_click_token do
    :crypto.strong_rand_bytes(16) |> Base.url_encode64(padding: false)
  end
end
