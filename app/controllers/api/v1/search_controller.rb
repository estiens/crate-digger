class Api::V1::SearchController < ApplicationController

  def query
    query = URI.encode(search_params['query'])
    render json: 'Must pass a query' unless query
    tracks = SpotifySearcher.find_tracks(query: query)
    render json: map_tracks(tracks)
  end

  def get_recs
    id = search_params[:id]
    tracks = SpotifySearcher.find_recommendations(id: id, options: option_params.to_h)
    render json: map_tracks(tracks)
  end 

  private 

  def map_tracks(tracks)
    array = []
    return array unless tracks.present? 
    tracks.map { |t| {title: t.name, 
                      spotifyId: t.id,
                      isrc: t.external_ids.try(:dig, 'isrc'),
                      artist: t.artists.first.name,
                      album: t.album.name,
                      imageUrl: t.album&.images&.first.try(:dig, 'url'),
                      previewUrl: t.preview_url } }
  end

  def search_params
    params.permit(:query, :id)
  end

  def option_params
    params.permit(SpotifySearcher::OPTION_KEYS) 
  end
end