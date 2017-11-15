class Api::V1::SearchController < ApplicationController

  def query
    query = URI.encode(search_params['query'])
    render json: 'Must pass a query' unless query
    limit = search_params[:limit]
    tracks = SpotifySearcher.find_tracks(query: query, limit: limit)
    render json: map_tracks(tracks)
  end

  def get_recs
    id = search_params[:id]
    limit = search_params[:limit]
    tracks = SpotifySearcher.find_recommendations(id: id, options: option_params.to_h, limit: limit)
    render json: map_tracks(tracks)
  end 

  def get_features
    id = search_params[:id]
    features = SpotifySearcher.find_features(id: id)
    render json: map_features(features)
  end

  private 

  def map_features(features)
    hash = {}
    return hash unless features.present? 
    keys = ['acousticness', 'danceability', 'duration_ms', 'energy',
    'instrumentalness', 'key', 'liveness', 'loudness', 'mode',
    'speechiness', 'tempo', 'time_signature', 'valence' ]
    keys.each { |key| hash[key] = features.send(key) }
    [hash]
  end

  def map_tracks(tracks)
    array = []
    return array unless tracks.present? 
    tracks.map { |t| {title: t.name, 
                      spotifyId: t.id,
                      isrc: t.external_ids.try(:dig, 'isrc'),
                      artist: t.artists.first.name,
                      album: t.album.name,
                      imageUrl: t.album&.images&.first.try(:dig, 'url'),
                      previewUrl: t.preview_url,
                      uri: t.uri } }
  end

  def search_params
    params.permit(:query, :id, :limit)
  end

  def option_params
    params.permit(SpotifySearcher::OPTION_KEYS) 
  end
end