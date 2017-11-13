require 'rspotify'

class SpotifySearcher
    OPTION_KEYS = [:target_acousticness, 
                   :target_danceability,
                   :target_duration,
                   :target_energy,
                   :target_instrumentalness,
                   :target_key,
                   :target_liveness,
                   :target_loudness,
                   :target_mode,
                   :target_popularity,
                   :target_speechiness,
                   :min_tempo,
                   :max_tempo,
                   :min_valence,
                   :max_valence
                 ].freeze
    
    #  [Float]         :min_acousticness Hard floor on the confidence measure from 0.0 to 1.0 of whether the track is acoustic. 1.0 represents high confidence the track is acoustic.          
    #  [Float]         :max_acousticness Hard ceiling on the confidence measure from 0.0 to 1.0 of whether the track is acoustic. 1.0 represents high confidence the track is acoustic. 
    #  [Float]         :target_acousticness Target value on the confidence measure from 0.0 to 1.0 of whether the track is acoustic. 1.0 represents high confidence the track is acoustic.
    #  [Float]         :min_danceability Hard floor on how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable.
    #  [Float]         :max_danceability Hard ceiling on how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable.
    #  [Float]         :target_danceability Target value on how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable.
    #  [Integer]       :min_duration_ms Hard floor on the duration of the track in milliseconds.
    #  [Integer]       :max_duration_ms Hard ceiling on the duration of the track in milliseconds.
    #  [Integer]       :target_duration_ms Target value on the duration of the track in milliseconds.
    #  [Float]         :min_energy Hard floor on energy which is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. For example, death metal has high energy, while a Bach prelude scores low on the scale. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy.
    #  [Float]         :max_energy Hard ceiling on energy which is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. For example, death metal has high energy, while a Bach prelude scores low on the scale. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy.
    #  [Float]         :target_energy Target value on energy which is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. For example, death metal has high energy, while a Bach prelude scores low on the scale. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy.
    #  [Float]         :min_instrumentalness Hard floor on prediction of whether a track contains no vocals. "Ooh" and "aah" sounds are treated as instrumental in this context. Rap or spoken word tracks are clearly "vocal". The closer the instrumentalness value is to 1.0, the greater likelihood the track contains no vocal content. Values above 0.5 are intended to represent instrumental tracks, but confidence is higher as the value approaches 1.0.
    #  [Float]         :max_instrumentalness Hard ceiling on prediction of whether a track contains no vocals. "Ooh" and "aah" sounds are treated as instrumental in this context. Rap or spoken word tracks are clearly "vocal". The closer the instrumentalness value is to 1.0, the greater likelihood the track contains no vocal content. Values above 0.5 are intended to represent instrumental tracks, but confidence is higher as the value approaches 1.0.
    #  [Float]         :target_instrumentalness Target value on prediction of whether a track contains no vocals. "Ooh" and "aah" sounds are treated as instrumental in this context. Rap or spoken word tracks are clearly "vocal". The closer the instrumentalness value is to 1.0, the greater likelihood the track contains no vocal content. Values above 0.5 are intended to represent instrumental tracks, but confidence is higher as the value approaches 1.0.
    #  [Integer]       :min_key Hard floor on the key the track is in. Integers map to pitches using standard {https://en.wikipedia.org/wiki/Pitch_class Pitch Class notation}. E.g. 0 = C, 1 = C♯/D♭, 2 = D, and so on.
    #  [Integer]       :max_key Hard ceiling on the key the track is in. Integers map to pitches using standard {https://en.wikipedia.org/wiki/Pitch_class Pitch Class notation}. E.g. 0 = C, 1 = C♯/D♭, 2 = D, and so on.   
    #  [Integer]       :target_key Target value on the key the track is in. Integers map to pitches using standard {https://en.wikipedia.org/wiki/Pitch_class Pitch Class notation}. E.g. 0 = C, 1 = C♯/D♭, 2 = D, and so on.   
    #  [Float]         :min_liveness Hard floor on liveness, which detects the presence of an audience in the recording. Higher liveness values represent an increased probability that the track was performed live. A value above 0.8 provides strong likelihood that the track is live.
    #  [Float]         :max_liveness Hard ceiling on liveness, which detects the presence of an audience in the recording. Higher liveness values represent an increased probability that the track was performed live. A value above 0.8 provides strong likelihood that the track is live.
    #  [Float]         :target_liveness Target value on liveness, which detects the presence of an audience in the recording. Higher liveness values represent an increased probability that the track was performed live. A value above 0.8 provides strong likelihood that the track is live.
    #  [Float]         :min_loudness Hard floor on the overall loudness of a track in decibels (dB). Loudness values are averaged across the entire track and are useful for comparing relative loudness of tracks. Loudness is the quality of a sound that is the primary psychological correlate of physical strength (amplitude). Values typical range between -60 and 0 db.
    #  [Float]         :max_loudness Hard ceiling on the overall loudness of a track in decibels (dB). Loudness values are averaged across the entire track and are useful for comparing relative loudness of tracks. Loudness is the quality of a sound that is the primary psychological correlate of physical strength (amplitude). Values typical range between -60 and 0 db.
    #  [Float]         :target_loudness Target value on the overall loudness of a track in decibels (dB). Loudness values are averaged across the entire track and are useful for comparing relative loudness of tracks. Loudness is the quality of a sound that is the primary psychological correlate of physical strength (amplitude). Values typical range between -60 and 0 db.
    #  [Integer]       :min_mode Hard floor on the modality (major or minor) of a track, the type of scale from which its melodic content is derived. Major is represented by 1 and minor is 0.         
    #  [Integer]       :max_mode Hard ceiling on the modality (major or minor) of a track, the type of scale from which its melodic content is derived. Major is represented by 1 and minor is 0.         
    #  [Integer]       :target_mode Target value on the modality (major or minor) of a track, the type of scale from which its melodic content is derived. Major is represented by 1 and minor is 0.         
    #  [Integer]       :min_popularity Hard floor on the popularity of the track. The value will be between 0 and 100, with 100 being the most popular. The popularity is calculated by algorithm and is based, in the most part, on the total number of plays the track has had and how recent those plays are.
    #  [Integer]       :max_popularity Hard ceiling on the popularity of the track. The value will be between 0 and 100, with 100 being the most popular. The popularity is calculated by algorithm and is based, in the most part, on the total number of plays the track has had and how recent those plays are.
    #  [Integer]       :target_popularity Target value on the popularity of the track. The value will be between 0 and 100, with 100 being the most popular. The popularity is calculated by algorithm and is based, in the most part, on the total number of plays the track has had and how recent those plays are.
    #  [Float]         :min_speechiness Hard floor on speechiness which detects the presence of spoken words in a track. The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value. Values above 0.66 describe tracks that are probably made entirely of spoken words. Values between 0.33 and 0.66 describe tracks that may contain both music and speech, either in sections or layered, including such cases as rap music. Values below 0.33 most likely represent music and other non-speech-like tracks.
    #  [Float]         :max_speechiness Hard ceiling on speechiness which detects the presence of spoken words in a track. The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value. Values above 0.66 describe tracks that are probably made entirely of spoken words. Values between 0.33 and 0.66 describe tracks that may contain both music and speech, either in sections or layered, including such cases as rap music. Values below 0.33 most likely represent music and other non-speech-like tracks.
    #  [Float]         :target_speechiness Target value on speechiness which detects the presence of spoken words in a track. The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value. Values above 0.66 describe tracks that are probably made entirely of spoken words. Values between 0.33 and 0.66 describe tracks that may contain both music and speech, either in sections or layered, including such cases as rap music. Values below 0.33 most likely represent music and other non-speech-like tracks.
    #  [Float]         :min_tempo Hard floor on the overall estimated tempo of a track in beats per minute (BPM). In musical terminology, tempo is the speed or pace of a given piece and derives directly from the average beat duration.
    #  [Float]         :max_tempo Hard ceiling on the overall estimated tempo of a track in beats per minute (BPM). In musical terminology, tempo is the speed or pace of a given piece and derives directly from the average beat duration.
    #  [Float]         :target_tempo Target value on the overall estimated tempo of a track in beats per minute (BPM). In musical terminology, tempo is the speed or pace of a given piece and derives directly from the average beat duration.
    #  [Integer]       :min_time_signature Hard floor on the estimated overall time signature of a track. The time signature (meter) is a notational convention to specify how many beats are in each bar (or measure).
    #  [Integer]       :max_time_signature Hard ceiling on the estimated overall time signature of a track. The time signature (meter) is a notational convention to specify how many beats are in each bar (or measure).
    #  [Integer]       :target_time_signature Target value on the estimated overall time signature of a track. The time signature (meter) is a notational convention to specify how many beats are in each bar (or measure).
    #  [Float]         :min_valence Hard floor on the measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry).
    #  [Float]         :max_valence Hard ceiling on the measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry).
    #  [Float]         :target_val

    def self.find_tracks(query:, limit: 5)
      return nil if query.length < 3
      RSpotify.authenticate(ENV['RSPOTIFY_TOKEN'], ENV['RSPOTIFY_PASSWORD'])
      tracks = RSpotify::Track.search(URI.decode(query), limit: limit, market: 'US')
      tracks.sort_by { |track| -track.popularity }
    end

    def self.find_recommendations(id:, limit: 10, options: {})
        RSpotify.authenticate(ENV['RSPOTIFY_TOKEN'], ENV['RSPOTIFY_PASSWORD'])
        options[:seed_tracks] = [id]
        options = options.symbolize_keys
        tracks = RSpotify::Recommendations.send(:generate, options).tracks
        tracks.sort_by { |track| -track.popularity }
    end
end