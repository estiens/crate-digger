defmodule CrateDiggerWeb.AffiliateController do
  use CrateDiggerWeb, :controller

  alias CrateDigger.Affiliate.Click
  alias CrateDigger.Repo

  import Ecto.Query

  @doc """
  Handle affiliate link redirects with click tracking.
  """
  def redirect(conn, %{"platform" => platform, "token" => token}) do
    # Find the pending click by token
    click = Repo.get_by(Click, token: token)

    if click do
      # Update click with timestamp and tracking info
      click
      |> Click.changeset(%{
        clicked_at: DateTime.utc_now(),
        ip_hash: hash_ip(conn),
        user_agent: get_user_agent(conn)
      })
      |> Repo.update()

      # Redirect to affiliate link
      Phoenix.Controller.redirect(conn, external: click.affiliate_link)
    else
      # Fallback: generate link on the fly based on platform
      fallback_url = get_fallback_url(platform)
      Phoenix.Controller.redirect(conn, external: fallback_url)
    end
  end

  defp hash_ip(conn) do
    ip =
      conn.remote_ip
      |> :inet.ntoa()
      |> to_string()

    :crypto.hash(:sha256, ip <> get_salt())
    |> Base.encode16()
    |> String.slice(0, 16)
  end

  defp get_salt do
    Application.get_env(:crate_digger, :ip_salt, "default_salt_change_in_prod")
  end

  defp get_user_agent(conn) do
    conn
    |> get_req_header("user-agent")
    |> List.first()
    |> case do
      nil -> "unknown"
      ua -> String.slice(ua, 0, 255)
    end
  end

  defp get_fallback_url("beatport"), do: "https://www.beatport.com"
  defp get_fallback_url("traxsource"), do: "https://www.traxsource.com"
  defp get_fallback_url("juno"), do: "https://www.junodownload.com"
  defp get_fallback_url("apple_music"), do: "https://music.apple.com"
  defp get_fallback_url("amazon"), do: "https://music.amazon.com"
  defp get_fallback_url("spotify"), do: "https://open.spotify.com"
  defp get_fallback_url(_), do: "/"
end
