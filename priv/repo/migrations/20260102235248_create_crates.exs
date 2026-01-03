defmodule CrateDigger.Repo.Migrations.CreateCrates do
  use Ecto.Migration

  def change do
    create table(:crates) do
      add :name, :string
      add :description, :text
      add :is_public, :boolean, default: false, null: false
      add :user_id, references(:users, on_delete: :nothing)

      timestamps(type: :utc_datetime)
    end

    create index(:crates, [:user_id])
  end
end
