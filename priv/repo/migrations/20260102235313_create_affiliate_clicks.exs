defmodule CrateDigger.Repo.Migrations.CreateAffiliateClicks do
  use Ecto.Migration

  def change do
    create table(:affiliate_clicks) do
      add :token, :string, null: false
      add :platform, :string, null: false
      add :affiliate_link, :string, null: false
      add :clicked_at, :utc_datetime
      add :converted, :boolean, default: false, null: false
      add :commission_amount, :decimal
      add :ip_hash, :string
      add :user_agent, :string
      add :track_id, references(:tracks, on_delete: :nothing)
      add :user_id, references(:users, on_delete: :nothing)

      timestamps(type: :utc_datetime)
    end

    create unique_index(:affiliate_clicks, [:token])
    create index(:affiliate_clicks, [:track_id])
    create index(:affiliate_clicks, [:user_id])
    create index(:affiliate_clicks, [:platform])
    create index(:affiliate_clicks, [:clicked_at])
  end
end
