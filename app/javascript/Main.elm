module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Html.Events exposing (..)
import Html.Events.Extra as Extra
import HttpBuilder exposing (..)
import Json.Decode as Decode exposing (Decoder, field, at)
import Json.Decode.Pipeline exposing (required, decode)
import Time
import Material
import Material.Slider as Slider
import Material.Progress as Loading
import Material.Card as Card
import Material.Options as Options exposing (cs, css)
import Material.Button as Button
import Material.Typography as Typo
import Material.List as Lists
import Material.Icon as Icon
import Toasty
import Toasty.Defaults
import Dict exposing (Dict)
import List.Extra


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias TrackInfo =
    { acousticness : Float
    , danceability : Float
    , duration_ms : Int
    , energy : Float
    , instrumentalness : Float
    , key : Int
    , liveness : Float
    , loudness : Float
    , mode : Int
    , speechiness : Float
    , tempo : Float
    , time_signature : Int
    , valence : Float
    }


type alias Track =
    { title : String
    , spotifyId : String
    , artist : String
    , album : String
    , imageUrl : Maybe String
    , previewUrl : Maybe String
    , uri : String
    }


type alias Model =
    { query : String
    , crate : List Track
    , tracks : List Track
    , recommendations : List Track
    , selectedTrack : List Track
    , showOptions : Bool
    , showCrate : Bool
    , showHistory : Bool
    , history : List Track
    , options : Dict String Float
    , mdl : Material.Model
    , tracksLoading : Bool
    , recsLoading : Bool
    , currentTrackInfo : List TrackInfo
    , toasties : Toasty.Stack Toasty.Defaults.Toast
    }


initialModel : Model
initialModel =
    { query = ""
    , crate = []
    , tracks = []
    , recommendations = []
    , selectedTrack = []
    , showOptions = False
    , showCrate = False
    , showHistory = False
    , history = []
    , options = Dict.empty
    , mdl = Material.model
    , tracksLoading = False
    , recsLoading = False
    , currentTrackInfo = []
    , toasties = Toasty.initialState
    }


myToastConfig : Toasty.Config Msg
myToastConfig =
    Toasty.Defaults.config
        |> Toasty.delay 3000



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
    | BackSearch
    | NewTrackInfo (Result Http.Error (List TrackInfo))
    | AddToCrate Track
    | DeleteFromCrate String
    | ClearCrate
    | ToastyMsg (Toasty.Msg Toasty.Defaults.Toast)
    | ToggleCrate
    | ToggleHistory


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToastyMsg subMsg ->
            Toasty.update myToastConfig ToastyMsg subMsg model

        AddToCrate track ->
            let
                message =
                    track.title ++ " was added to your crate"
            in
                ( { model | crate = List.append model.crate [ track ] }, Cmd.none )
                    |> addToast (Toasty.Defaults.Success "Added" message)

        DeleteFromCrate string ->
            let
                newCrate =
                    List.Extra.dropWhile (\track -> track.spotifyId == string) model.crate
            in
                ( { model | crate = newCrate }, Cmd.none )

        ClearCrate ->
            ( { model | crate = [] }, Cmd.none )

        Slider key value ->
            let
                newOptions =
                    Dict.insert key value model.options
            in
                ( { model | options = newOptions }, Cmd.none )

        UpdateQuery string ->
            ( { model | query = string }, Cmd.none )

        BackSearch ->
            ( { model | selectedTrack = [], showOptions = False, recommendations = [] }, Cmd.none )

        Search ->
            ( { model | selectedTrack = [], tracks = [], recommendations = [], tracksLoading = True, showOptions = False, options = Dict.empty }, searchForTracks model.query )

        FilterSearch ->
            let
                track =
                    List.head model.selectedTrack
            in
                case track of
                    Nothing ->
                        ( model, Cmd.none )

                    Just track ->
                        ( { model | recsLoading = True, tracks = [], recommendations = [] }, searchForRecs track.spotifyId model.options )

        ResetOptions ->
            ( { model | options = Dict.empty }, Cmd.none )

        NewTracks (Ok newTracks) ->
            ( { model | tracksLoading = False, tracks = newTracks }, Cmd.none )

        NewTracks (Err error) ->
            let
                _ =
                    Debug.log "Oops" error
            in
                ( model, Cmd.none )

        ToggleCrate ->
            ( { model | showCrate = not model.showCrate }, Cmd.none )

        ToggleHistory ->
            ( { model | showHistory = not model.showHistory }, Cmd.none )

        GetRecommendations track ->
            ( { model | recsLoading = True, currentTrackInfo = [], history = List.append model.history [ track ], tracks = [], recommendations = [], showHistory = False, selectedTrack = [ track ] }
            , Cmd.batch [ searchForRecs track.spotifyId model.options, getTrackInfo track.spotifyId ]
            )

        NewRecommendations (Ok newRecs) ->
            ( { model | recsLoading = False, showOptions = True, recommendations = newRecs }, Cmd.none )

        NewRecommendations (Err error) ->
            let
                _ =
                    Debug.log "Oops" error
            in
                ( model, Cmd.none )

        NewTrackInfo (Ok newInfo) ->
            ( { model | currentTrackInfo = newInfo }, Cmd.none )

        NewTrackInfo (Err error) ->
            let
                _ =
                    Debug.log "Oops" error
            in
                ( model, Cmd.none )

        Mdl msg_ ->
            Material.update Mdl msg_ model



-- COMMANDS


addToast : Toasty.Defaults.Toast -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToast toast ( model, cmd ) =
    Toasty.addToast myToastConfig ToastyMsg toast ( model, cmd )


searchForRecs : String -> Dict String Float -> Cmd Msg
searchForRecs id options =
    let
        base_url =
            "/api/v1/recommendations"

        optionsDict =
            Dict.map (\key value -> toString (value)) options

        options_array =
            Dict.toList optionsDict

        query_params =
            options_array ++ [ ( "id", id ) ]
    in
        HttpBuilder.get base_url
            |> withQueryParams query_params
            |> withTimeout (10 * Time.second)
            |> withExpect (Http.expectJson tracksDecoder)
            |> send NewRecommendations


getTrackInfo : String -> Cmd Msg
getTrackInfo id =
    let
        url =
            "/api/v1/features?id=" ++ id
    in
        HttpBuilder.get url
            |> withTimeout (10 * Time.second)
            |> withExpect (Http.expectJson infosDecoder)
            |> send NewTrackInfo


searchForTracks : String -> Cmd Msg
searchForTracks query =
    let
        url =
            "/api/v1/track_search?limit=4&query=" ++ query
    in
        HttpBuilder.get url
            |> withTimeout (10 * Time.second)
            |> withExpect (Http.expectJson tracksDecoder)
            |> send NewTracks



-- VIEW


type alias Mdl =
    Material.Model


historyItem : Track -> Html Msg
historyItem track =
    Lists.li [ Options.cs "history-item" ] [ Lists.content [ Options.onClick (GetRecommendations track) ] [ text track.title ] ]


historyBox : Model -> Html Msg
historyBox model =
    let
        history =
            takeFromList 5 (List.reverse model.history)
    in
        div [ hidden (not model.showHistory) ]
            [ Lists.ul [ Options.cs "history-box" ] (List.map historyItem history)
            ]


selectedTrack : Maybe Track -> Html Msg
selectedTrack track =
    case track of
        Nothing ->
            div [ class "selected-track-box" ] []

        Just track ->
            let
                trackString =
                    track.title ++ " by " ++ track.artist
            in
                div [ class "selected-track-box" ]
                    [ case track.imageUrl of
                        Nothing ->
                            img [] []

                        Just val ->
                            div [ class "selected-track-info-box" ]
                                [ img [ src (val) ] []
                                , h2 [] [ text trackString ]
                                ]
                    ]


showSelectedTrack : Model -> Html Msg
showSelectedTrack model =
    let
        track =
            List.head model.selectedTrack

        info =
            List.head model.currentTrackInfo
    in
        div [] [ selectedTrack track, trackInfo info ]


trackInfo : Maybe TrackInfo -> Html Msg
trackInfo info =
    case info of
        Nothing ->
            div [ class "acoustic-info-box" ] []

        Just info ->
            div [ class "acoustic-info-box" ]
                [ Lists.ul []
                    [ Lists.li [ Lists.withSubtitle ]
                        [ Lists.content []
                            [ text "Acousticness"
                            , Lists.subtitle [] [ text (toString info.acousticness) ]
                            ]
                        ]
                    , Lists.li [ Lists.withSubtitle ]
                        [ Lists.content []
                            [ text "Danceability"
                            , Lists.subtitle [] [ text (toString info.danceability) ]
                            ]
                        ]
                    , Lists.li [ Lists.withSubtitle ]
                        [ Lists.content []
                            [ text "Loudness (db)"
                            , Lists.subtitle [] [ text (toString info.loudness) ]
                            ]
                        ]
                    , Lists.li [ Lists.withSubtitle ]
                        [ Lists.content []
                            [ text "Liveness"
                            , Lists.subtitle [] [ text (toString info.liveness) ]
                            ]
                        ]
                    , Lists.li [ Lists.withSubtitle ]
                        [ Lists.content []
                            [ text "Energy"
                            , Lists.subtitle [] [ text (toString info.energy) ]
                            ]
                        ]
                    , Lists.li [ Lists.withSubtitle ]
                        [ Lists.content []
                            [ text "Instrumentalness"
                            , Lists.subtitle [] [ text (toString info.instrumentalness) ]
                            ]
                        ]
                    , Lists.li [ Lists.withSubtitle ]
                        [ Lists.content []
                            [ text "Speechiness"
                            , Lists.subtitle [] [ text (toString info.speechiness) ]
                            ]
                        ]
                    , Lists.li [ Lists.withSubtitle ]
                        [ Lists.content []
                            [ text "Mood (0 = sad, 1 = happy)"
                            , Lists.subtitle [] [ text (toString info.valence) ]
                            ]
                        ]
                    , Lists.li [ Lists.withSubtitle ]
                        [ Lists.content []
                            [ text ("Tempo (BPM): " ++ toString (info.tempo)) ]
                        ]
                    , Lists.li [ Lists.withSubtitle ]
                        [ Lists.content []
                            [ text ("Time Signature: " ++ toString (info.time_signature)) ]
                        ]
                    , Lists.li [ Lists.withSubtitle ]
                        [ Lists.content []
                            [ text ("Key: " ++ mapKey (toFloat (info.key))) ]
                        ]
                    ]
                ]


audiobox : Maybe String -> Html Msg
audiobox string =
    case string of
        Nothing ->
            div [] []

        Just url ->
            div [ class "card-audio" ]
                [ video [ attribute "controls" "", attribute "controlsList" "nodownload", width 150, name "media" ]
                    [ source [ src url, type_ "audio/mpeg" ]
                        []
                    ]
                ]


trackBox : Track -> Html Msg
trackBox track =
    case track.imageUrl of
        Nothing ->
            div [ class "trackbox" ] [ text "no image" ]

        Just url ->
            let
                backgroundUrl =
                    "url('" ++ url ++ "') center / cover"

                spotifyUrl =
                    "https://open.spotify.com/embed?uri=spotify:track:" ++ track.spotifyId
            in
                div [ class "trackbox" ]
                    [ Card.view
                        [ Options.onClick (GetRecommendations track) ]
                        [ Card.media
                            [ css "background" backgroundUrl
                            , css "height" "200px"
                            ]
                            []
                        , Card.title []
                            [ Card.head [] [ text track.artist ]
                            , Card.subhead [] [ text track.title, br [] [], text track.album ]
                            ]
                        , Card.menu []
                            [ div [] [ audiobox track.previewUrl ]
                            ]
                        , Card.actions
                            []
                            [ div [ onClick (AddToCrate track) ] [ Icon.i "favorite_border" ]
                            , div [ class "spotify-player-button" ] [ a [ href track.uri ] [ img [ attribute "width" "25px", src "/assets/Spotify_Icon_RGB_Black.png" ] [] ] ]
                            ]
                        ]
                    ]


viewTracks : String -> List Track -> Html Msg
viewTracks header tracks =
    div [ hidden (List.isEmpty tracks) ]
        [ br [] []
        , Options.styled Html.h2
            [ Typo.display2 ]
            [ text header ]
        , div [ class "cards" ] (List.map trackBox tracks)
        ]


createSlider : Float -> Float -> Float -> Float -> String -> Model -> Html Msg
createSlider min_slider max_slider default_slider step attribute model =
    Slider.view
        [ Slider.onChange (Slider attribute)
        , Slider.value (getDef attribute default_slider model.options)
        , Slider.max max_slider
        , Slider.min min_slider
        , Slider.step step
        ]


mapKey : Float -> String
mapKey float =
    case float of
        (-1) ->
            "None set"

        0 ->
            "C"

        1 ->
            "C♯"

        2 ->
            "D"

        3 ->
            "D♯"

        4 ->
            "E"

        5 ->
            "F"

        6 ->
            "F♯"

        7 ->
            "G"

        8 ->
            "G♯"

        9 ->
            "A"

        10 ->
            "B♭"

        11 ->
            "B"

        _ ->
            "Nothing Set"


showKey : Maybe Float -> String
showKey maybefloat =
    case maybefloat of
        Nothing ->
            "None Set"

        Just val ->
            mapKey val


showSlider : Float -> Float -> Float -> Float -> String -> String -> Model -> Html Msg
showSlider min_slider max_slider default_slider step attribute label model =
    let
        displayValue =
            case attribute of
                "target_key" ->
                    showKey (Dict.get attribute model.options)

                _ ->
                    sliderValue (Dict.get attribute model.options)

        opacity =
            case Dict.get attribute model.options of
                Nothing ->
                    "0.5"

                Just val ->
                    "1.0"
    in
        div [ class "slider", style [ ( "opacity", opacity ) ] ]
            [ createSlider min_slider max_slider default_slider step attribute model
            , p [] [ text (label ++ ": " ++ displayValue) ]
            ]


trackOptions : Model -> Html Msg
trackOptions model =
    div [ hidden (not model.showOptions) ]
        [ div [ class "sliders" ]
            [ Options.styled p
                [ Typo.display1, Typo.center ]
                [ text "Select You Some Magic Filters" ]
            , div [ class "row" ]
                [ showSlider 0 1 0.5 0.1 "target_acousticness" "target acousticness" model
                , showSlider 0 1 0.5 0.1 "target_energy" "target energy" model
                , showSlider 0 1 0.5 0.1 "target_instrumentalness" "instrumentalness" model
                , showSlider 0 1 0.5 0.1 "target_liveness" "likely to be live" model
                , showSlider -60 0 -20 1 "target_loudness" "loudness" model
                , showSlider 0 1 0.5 0.1 "target_speechiness" "speechiness" model
                , showSlider 0 100 50 10 "target_popularity" "popularity" model
                , showSlider 0 1 0.5 0.1 "target_danceability" "danceability" model
                , showSlider 0 200 100 1 "min_tempo" "min tempo (BPM)" model
                , showSlider 0 200 140 1 "max_tempo" "max tempo (BPM)" model
                , showSlider 0 1 0.5 0.1 "min_valence" "min valence 0-sad, 1-happy)" model
                , showSlider 0 1 0.5 0.1 "max_valence" "max valence (0-sad, 1-happy)" model
                , showSlider -1 11 -1 1 "target_key" "key" model
                ]
            , Button.render Mdl
                [ 0 ]
                model.mdl
                [ Button.raised
                , Button.colored
                , Options.onClick FilterSearch
                ]
                [ text "Search With Filters" ]
            , Button.render Mdl
                [ 1 ]
                model.mdl
                [ Button.raised
                , Button.colored
                , Options.onClick ResetOptions
                ]
                [ text "Reset Options" ]
            ]
        ]


showLoader : Model -> Html Msg
showLoader model =
    Loading.indeterminate


mainContent : Model -> Html Msg
mainContent model =
    div [ class "mdl-layout mdl-js-layout mdl-layout--fixed-header" ]
        [ header [ class "mdl-layout__header" ]
            [ div [ class "mdl-layout__header-row" ]
                [ span [ class "mdl-layout-title" ]
                    [ text "CRA8digger" ]
                , div [ class "mdl-layout-spacer" ]
                    []
                , nav [ class "mdl-navigation mdl-layout--large-screen-only" ]
                    [ resetButton model
                    , crateButton model
                    , historyButton model
                    ]
                ]
            ]
        , historyBox model
        , main_ [ class "mdl-layout__content" ]
            [ div []
                [ div [ class "search-bar", hidden (not (List.isEmpty model.selectedTrack)) ]
                    [ Options.styled Html.h2 [ Typo.display2 ] [ text "Select a Seed Track" ]
                    , input [ class "search-input", placeholder "Search For Tracks", onInput UpdateQuery, Extra.onEnter Search ] []
                    , Button.render Mdl
                        [ 0 ]
                        model.mdl
                        [ Button.raised
                        , Button.colored
                        , Options.onClick Search
                        ]
                        [ text "Search Tracks" ]
                    ]
                , div [ class "selected-track", hidden (List.isEmpty model.selectedTrack) ] [ showSelectedTrack model ]
                , div [ class "crate-list" ]
                    [ if model.showCrate == False then
                        div [] []
                      else
                        viewTracks "Your Crate" model.crate
                    , clearCrateButton model
                    ]
                , div [ class "track-list" ]
                    [ if model.tracksLoading == True then
                        div []
                            [ p [] [ text "Searching For Your Track" ]
                            , showLoader model
                            ]
                      else if model.showCrate == True then
                        div [] []
                      else
                        viewTracks "Track List" model.tracks
                    ]
                , div [ class "option-box" ] [ trackOptions model ]
                , div [ class "recommendation-list" ]
                    [ if model.recsLoading == True then
                        div []
                            [ p [] [ text "Searching For Recommendations" ]
                            , showLoader model
                            ]
                      else
                        viewTracks "Recommendation List" model.recommendations
                    ]
                , Toasty.view myToastConfig Toasty.Defaults.view ToastyMsg model.toasties
                ]
            ]
        ]


historyButton : Model -> Html Msg
historyButton model =
    let
        buttonText =
            if model.showHistory == True then
                "Hide History"
            else
                "Show History"
    in
        a [ class "mdl-navigation__link", onClick ToggleHistory ]
            [ text buttonText ]


resetButton : Model -> Html Msg
resetButton model =
    a [ class "mdl-navigation__link", onClick BackSearch, hidden (List.isEmpty model.selectedTrack) ]
        [ text "Back to Track Search" ]


crateButton : Model -> Html Msg
crateButton model =
    let
        buttonText =
            if model.showCrate == True then
                "Hide Crate"
            else
                "View My Crate (" ++ toString (List.length model.crate) ++ ")"
    in
        a [ class "mdl-navigation__link", onClick ToggleCrate, hidden (List.isEmpty model.crate) ]
            [ text buttonText ]


clearCrateButton : Model -> Html Msg
clearCrateButton model =
    div [ class "clear-create-button", hidden (List.isEmpty model.crate) ]
        [ Button.render Mdl
            [ 0 ]
            Material.model
            [ Button.raised
            , Button.colored
            , Button.ripple
            , Options.onClick ClearCrate
            ]
            [ text "Empty Crate" ]
        ]


headerText : Model -> Html Msg
headerText model =
    Options.styled Html.h2
        [ Typo.display4 ]
        [ text "CR8digger" ]


view : Model -> Html Msg
view model =
    mainContent model



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


infosDecoder : Decode.Decoder (List TrackInfo)
infosDecoder =
    Decode.list infoDecoder


infoDecoder : Decoder TrackInfo
infoDecoder =
    decode TrackInfo
        |> Json.Decode.Pipeline.required "acousticness" Decode.float
        |> Json.Decode.Pipeline.required "danceability" Decode.float
        |> Json.Decode.Pipeline.required "duration_ms" Decode.int
        |> Json.Decode.Pipeline.required "energy" Decode.float
        |> Json.Decode.Pipeline.required "instrumentalness" Decode.float
        |> Json.Decode.Pipeline.required "key" Decode.int
        |> Json.Decode.Pipeline.required "liveness" Decode.float
        |> Json.Decode.Pipeline.required "loudness" Decode.float
        |> Json.Decode.Pipeline.required "mode" Decode.int
        |> Json.Decode.Pipeline.required "speechiness" Decode.float
        |> Json.Decode.Pipeline.required "tempo" Decode.float
        |> Json.Decode.Pipeline.required "time_signature" Decode.int
        |> Json.Decode.Pipeline.required "valence" Decode.float


tracksDecoder : Decode.Decoder (List Track)
tracksDecoder =
    Decode.list trackDecoder


trackDecoder : Decoder Track
trackDecoder =
    Decode.map7 Track
        (field "title" Decode.string)
        (field "spotifyId" Decode.string)
        (field "artist" Decode.string)
        (field "album" Decode.string)
        (Decode.maybe <| Decode.field "imageUrl" Decode.string)
        (Decode.maybe <| Decode.field "previewUrl" Decode.string)
        (field "uri" Decode.string)



-- HELPERS


sliderValue : Maybe a -> String
sliderValue maybeNum =
    case maybeNum of
        Nothing ->
            "not set"

        Just val ->
            toString val


takeFromList : Int -> List a -> List a
takeFromList n list =
    if n <= 0 then
        []
    else
        case list of
            [] ->
                []

            x :: xs ->
                x :: takeFromList (n - 1) xs
