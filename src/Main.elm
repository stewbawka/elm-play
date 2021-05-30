module Main exposing (main)

import Browser
import Http
import Html exposing (..)
import Html.Attributes exposing (class, disabled, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Json.Decode exposing (Decoder, at, bool, int, list, string, succeed)
import Json.Decode.Pipeline exposing (required)

type alias Id =
    String


type alias Track =
    { id : Id
    , artist : String
    , title : String
    , bpm : Int
    , isEdit : Bool
    , originalYear : Int
    , year : Int
    }

type alias TrackList =
    List Track

type alias Model =
    { trackList : Maybe TrackList }

trackDecoder : Decoder Track
trackDecoder =
    succeed Track
        |> required "id" string
        |> required "artist" string
        |> required "title" string
        |> required "bpm" int
        |> required "is_edit" bool
        |> required "original_year" int
        |> required "year" int

trackListDecoder : Decoder (List Track)
trackListDecoder =
    at ["data"] (list trackDecoder)


baseUrl : String
baseUrl =
    "http://localhost:4000/api"


initialModel : Model
initialModel =
    { trackList = Nothing }

fetchTrack : Cmd Msg
fetchTrack =
    Http.get
        { url = baseUrl ++ "/tracks"
        , expect = Http.expectJson LoadTrackList trackListDecoder
        }
    
init : () -> ( Model, Cmd Msg )
init () =
    ( initialModel, fetchTrack )


viewDetailedTrack :
    Track
    -> Html Msg
viewDetailedTrack track =
    div [ class "detailed-track" ]
        [ div [ class "photo-info" ]
            [ h2 [ class "caption" ] [ text ( String.fromInt track.bpm ) ]
            , h2 [ class "caption" ] [ text track.artist ]
            , h2 [ class "caption" ] [ text track.title ]
            ]
        ]
viewTrackList : Maybe TrackList -> Html Msg
viewTrackList maybeTrackList =
    case maybeTrackList of
        Just trackList ->
            div [] (List.map viewDetailedTrack trackList)

        Nothing ->
            div [ class "loading-feed" ]
                [ text "Loading track list..." ]

view : Model -> Html Msg
view model =
    div []
        [ div [ class "header" ]
            [ h1 [] [ text "Rekkids" ] ]
        , div [ class "content-flow" ]
            [ viewTrackList model.trackList ]
        ]


type Msg
    = LoadTrackList (Result Http.Error TrackList)


update :
    Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadTrackList (Ok trackList) ->
            ( { model | trackList = Just trackList }
            , Cmd.none )
        LoadTrackList (Err _) ->
            ( model, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
