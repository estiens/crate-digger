# routes.rb

Rails.application.routes.draw do
  root to: "application#index"

  namespace :api do
    namespace :v1 do
      get 'track_search', to: 'search#query'
      get 'recommendations', to: 'search#get_recs'
    end
  end
end
