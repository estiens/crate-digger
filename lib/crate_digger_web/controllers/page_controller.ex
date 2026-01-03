defmodule CrateDiggerWeb.PageController do
  use CrateDiggerWeb, :controller

  def home(conn, _params) do
    render(conn, :home)
  end
end
