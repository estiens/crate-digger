import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import HttpBuilder exposing (..)
import Json.Decode as Decode exposing (Decoder, field, at)

import Material
import Material.Slider as Slider
import Material.Progress as Loading
import Material.Card as Card
import Material.Options as Options exposing (cs, css)
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Button as Button

import Dict exposing (Dict)

main : Program Never Model Msg
main =
  Html.program
    { init = ( initialModel, Cmd.none)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type alias Track =
    { title: String 
    , spotifyId : String
    , artist : String
    , album : String
    , imageUrl : Maybe String
    , previewUrl : Maybe String
    }

type alias Model =
  { query : String
  , tracks : List Track
  , recommendations: List Track
  , selectedTrack : List Track
  , showOptions : Bool
  , options: Dict String Float
  , mdl: Material.Model,
    tracksLoading: Bool,
    recsLoading: Bool
  }

initialModel : Model
initialModel =
    { query = "",
      tracks = [],
      recommendations = [],
      selectedTrack = [],
      showOptions = False,
      options = Dict.empty,
      mdl = Material.model,
      tracksLoading = False,
      recsLoading = False
    }

-- UPDATE

type Msg
  = Search 
  | NewTracks (Result Http.Error (List Track))
  | NewRecommendations (Result Http.Error (List Track))
  | GetRecommendations Track
  | Mdl (Material.Msg Msg)
  | Slider String Float
  | ResetOptions
  | FilterSearch
  | UpdateQuery String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Slider key value ->
      let
        newOptions = Dict.insert key value model.options
      in
        ( { model | options = newOptions }, Cmd.none )
    UpdateQuery string ->
      ( { model | query = string }, Cmd.none )
    Search ->
      ( { model | selectedTrack = [], tracks = [], recommendations = [], tracksLoading = True, showOptions = False, options = Dict.empty }, searchForTracks model.query)
    FilterSearch ->
      let
        track = fromJust(List.head model.selectedTrack)
      in
        ( { model | recsLoading = True, tracks = [], recommendations = [] }, searchForRecs track.spotifyId model.options )
    ResetOptions ->
      ( { model | options = Dict.empty }, Cmd.none )
    NewTracks (Ok newTracks) ->
      ( { model | tracksLoading = False, tracks = newTracks }, Cmd.none )
    NewTracks (Err error) ->
      let
        _ = Debug.log "Oops" error
      in
        (model, Cmd.none)
    GetRecommendations track ->
      ( { model | recsLoading = True, tracks = [], recommendations = [], selectedTrack = [track] }, searchForRecs track.spotifyId model.options )
    NewRecommendations (Ok newRecs) ->
      ( { model | recsLoading = False, showOptions = True, recommendations = newRecs }, Cmd.none )
    NewRecommendations (Err error) ->
      let
        _ = Debug.log "Oops" error
      in
        (model, Cmd.none)
    Mdl msg_ ->
      Material.update Mdl msg_ model

-- COMMANDS

searchForRecs : String -> Dict String Float -> Cmd Msg
searchForRecs id options =
    let
      base_url = "/api/v1/recommendations"
      optionsDict = Dict.map (\key value -> toString(value)) options
      options_array = Dict.toList optionsDict 
      query_params = options_array ++ [("id", id)]
    in
      HttpBuilder.get base_url
        |> withQueryParams query_params
        |> withExpect (Http.expectJson tracksDecoder)
        |> send NewRecommendations

searchForTracks : String -> Cmd Msg
searchForTracks query =
    let
      url = "/api/v1/track_search?limit=4&query=" ++ query
    in
      Decode.list trackDecoder
        |> Http.get url
        |> Http.send NewTracks

-- VIEW

type alias Mdl = Material.Model

selectedTrack : Track -> Html Msg 
selectedTrack track = 
  div [] [
    h2 [] [text("Selected Track: " ++ track.title ++ " by " ++ track.artist)]
  ]

showSelectedTrack : Model -> Html Msg 
showSelectedTrack model =
  let 
    track = List.head model.selectedTrack
  in
    case track of 
      Nothing ->
        div [] []
      Just val ->
        selectedTrack(fromJust(track))

audiobox : Maybe String -> Html Msg 
audiobox string =
  case string of 
    Nothing ->
      div [] []
    Just url ->
    div [ class "card-audio"]
        [ video [ attribute "controls" "", width 150, name "media" ]
          [ source [ src url, type_ "audio/mpeg" ]
            []
          ]
        ]

trackBox : Track -> Html Msg
trackBox track =
  case track.imageUrl of 
    Nothing ->
      div [ class "trackbox"] [ text "no image" ]
    Just url ->
      let 
        backgroundUrl = "url('" ++ url ++ "') center / cover"
      in
      div  [ class "trackbox"] [
        Card.view 
          [Color.background (Color.color Color.Green Color.S500)
          , Options.onClick(GetRecommendations track)] 
          [
            Card.media [  css "background" backgroundUrl
                        , css "height" "200px"] []
            ,Card.title [] [ 
              Card.head [ ] [ text track.artist ] 
              ,Card.subhead [ ] [text track.title, br [] [], text track.album ]
            ]
          , Card.menu [] [
            div [] [audiobox track.previewUrl]
        ]   ]
      ] 

viewTracks : String -> List Track -> Html Msg
viewTracks header tracks =
    div [hidden (List.isEmpty tracks)] [ 
      br [] []
      , h1 [] [text header]
      , div [class "cards"] (List.map trackBox tracks)
    ]

createSlider: Float -> Float -> Float -> Float -> String -> Model -> Html Msg
createSlider min_slider max_slider default_slider step attribute model =
  Slider.view
    [ Slider.onChange (Slider attribute)
    , Slider.value (getDef attribute default_slider model.options)
    , Slider.max max_slider
    , Slider.min min_slider
    , Slider.step step
  ]

showKey: Maybe Float -> String 
showKey maybefloat =
  case maybefloat of 
    Nothing -> "None Set"
    Just val ->
      case val of 
        -1 -> "None set"
        0 -> "c"
        1 -> "C♯"
        2 -> "D"
        3 -> "D♯"
        4 -> "E"
        5 -> "F"
        6 ->  "F♯"
        7 ->  "G"
        8 ->  "G♯"
        9 ->  "A"
        10 -> "B♭"
        11 -> "B"
        _ -> "Nothing Set"

showSlider: Float -> Float -> Float -> Float -> String -> String -> Model -> Html Msg
showSlider min_slider max_slider default_slider step attribute label model =
  div [class "slider"] [
        createSlider min_slider max_slider default_slider step attribute model,
        p [] [text(label ++ ": " ++ sliderValue(Dict.get attribute model.options))]
        ]
    
trackOptions: Model -> Html Msg
trackOptions model =
  Options.div[ Elevation.e4, Options.center ] [
    div [hidden (not model.showOptions)] [
      div [class "sliders"] [
        showSlider 0 1 0.5 0.1 "target_acousticness" "target acousticness" model,
        showSlider 0 1 0.5 0.1 "target_energy" "target energy" model,
        showSlider 0 1 0.5 0.1 "target_instrumentalness" "instrumentalness" model,
        showSlider 0 1 0.5 0.1 "target_liveness" "likely to be live" model,
        showSlider 0 1 0.5 0.1 "target_loudness" "loudness" model,
        showSlider 0 1 0.5 0.1 "target_speechiness" "speechiness" model,
        showSlider 0 100 50 10 "target_popularity" "popularity" model,
        showSlider 0 1 0.5 0.1 "target_danceability" "danceability" model,
        showSlider 0 200 100 1 "min_tempo" "min tempo (BPM)" model,
        showSlider 0 200 140 1 "max_tempo" "max tempo (BPM)" model,
        showSlider 0 1 0.5 0.1 "min_valence" "min valence 0-sad, 1-happy)" model,
        showSlider 0 1 0.5 0.1 "max_valence" "max valence (0-sad, 1-happy)" model,
        div [class "slider"] [
            createSlider -1 11 -1 1 "target_key" model,
            p [] [text("key" ++ ": " ++ showKey(Dict.get "target_key" model.options))]
        ],
      Button.render Mdl [0] model.mdl
        [ Button.raised
        , Button.colored
        , Options.onClick FilterSearch
      ]
      [ text "Search With Filters" ],
      Button.render Mdl [1] model.mdl
        [ Button.raised
        , Button.colored
        , Options.onClick ResetOptions
      ]
      [ text "Reset Options" ]
      ]
    ]
  ]
showLoader: Model -> Html Msg
showLoader model =
  Loading.indeterminate

mainContent: Model -> Html Msg
mainContent model =
  div [] [ div [class "search-bar"] [
    input [ placeholder "Search For Tracks", onInput UpdateQuery ] []
    , button [onClick Search] [text "Search Tracks"]
  ]
  , div [class "selected-track"] [ showSelectedTrack model ]
  , div [class "track-list"] [
    if model.tracksLoading == True then
      showLoader model
    else
      viewTracks "Track List" model.tracks

    ]
  , div [class "recommendation-list"] [
    trackOptions model
    ,if model.recsLoading == True then
      showLoader model
    else
      viewTracks "Recommendation List" model.recommendations
    ]
  ] 

header : Model -> Html Msg
header model =
    div [] 
      [ h1 [] [text "CRATE DIGGER"] ]

view : Model -> Html Msg
view model =
    div []
        [ header model
        , mainContent model
        ]

-- SLIDER COMMANDS 

get : String -> Dict String Float -> Float
get key dict =
  Dict.get key dict |> Maybe.withDefault 0


getDef : String -> Float -> Dict String Float -> Float
getDef key def dict =
  Dict.get key dict |> Maybe.withDefault def

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- DECODERS

tracksDecoder : Decode.Decoder (List Track)
tracksDecoder =
    Decode.list trackDecoder

trackDecoder : Decoder Track 
trackDecoder =
  Decode.map6 Track
    (field  "title" Decode.string)
    (field  "spotifyId" Decode.string)
    (field  "artist" Decode.string)
    (field  "album" Decode.string)
    (Decode.maybe <| Decode.field "imageUrl" Decode.string)
    (Decode.maybe <| Decode.field "previewUrl" Decode.string)

-- HELPERS

fromJust : Maybe a -> a
fromJust x = case x of
    Just y -> y
    Nothing -> Debug.crash "error: fromJust Nothing"

sliderValue : Maybe a -> String 
sliderValue maybeNum =
  case maybeNum of                  
    Nothing ->                              
      "Nothing Set"
    Just val ->                             
      toString val
 


