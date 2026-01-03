defmodule CrateDigger.Affiliate.Click do
  use Ecto.Schema
  import Ecto.Changeset

  schema "affiliate_clicks" do
    field :token, :string
    field :platform, :string
    field :affiliate_link, :string
    field :clicked_at, :utc_datetime
    field :converted, :boolean, default: false
    field :commission_amount, :decimal
    field :ip_hash, :string
    field :user_agent, :string
    field :track_id, :id
    field :user_id, :id

    timestamps(type: :utc_datetime)
  end

  @doc false
  def changeset(click, attrs) do
    click
    |> cast(attrs, [:token, :platform, :affiliate_link, :clicked_at, :converted, :commission_amount, :ip_hash, :user_agent, :track_id, :user_id])
    |> validate_required([:platform, :affiliate_link])
    |> generate_token()
  end

  defp generate_token(changeset) do
    if get_field(changeset, :token) do
      changeset
    else
      put_change(changeset, :token, random_token())
    end
  end

  defp random_token do
    :crypto.strong_rand_bytes(16) |> Base.url_encode64(padding: false)
  end
end
