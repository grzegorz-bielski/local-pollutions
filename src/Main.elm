module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, header, img, li, p, pre, span, text, ul)
import Html.Attributes exposing (src)
import Http
import Json.Decode as D
import Json.Decode.Pipeline as DP
import List



---- MODEL ----


type Model
    = Error
    | Loading
    | Cities (List City)


type Country
    = Germany
    | Poland


type alias City =
    { city : String
    , country : Country
    , locations : Int
    , count : Int
    }


citiesDecoder : D.Decoder (List City)
citiesDecoder =
    D.field "results" (D.list cityDecoder)


cityDecoder : D.Decoder City
cityDecoder =
    D.succeed City
        |> DP.required "city" D.string
        |> DP.required "country" countryDecoder
        |> DP.required "locations" D.int
        |> DP.required "count" D.int


countryCode : Country -> String
countryCode country =
    case country of
        Germany ->
            "DE"

        Poland ->
            "PL"


countryDecoder : D.Decoder Country
countryDecoder =
    let
        decodeContry str =
            case str of
                "DE" ->
                    D.succeed Germany

                "PL" ->
                    D.succeed Poland

                other ->
                    D.fail <| "Unkown country: " ++ other
    in
    D.string |> D.andThen decodeContry


getPollutedCities : Country -> Cmd Msg
getPollutedCities country =
    let
        url =
            "https://api.openaq.org/v1/cities?country="
                ++ countryCode country
                ++ "&order_by=count&sort=desc&limit=10"
    in
    Http.get { url = url, expect = Http.expectJson GotCities citiesDecoder }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading, getPollutedCities Germany )



---- UPDATE ----


type Msg
    = GotCities (Result Http.Error (List City))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCities res ->
            case res of
                Ok cities ->
                    ( Cities cities, Cmd.none )

                Err _ ->
                    ( Error, Cmd.none )



---- VIEW ----


headerView : Html Msg
headerView =
    header []
        [ img [ src "/logo.svg" ] []
        ]


contentView : Model -> Html Msg
contentView model =
    case model of
        Error ->
            span [] [ text "Unable to load the asset" ]

        Loading ->
            span [] [ text "loading..." ]

        Cities citiesList ->
            citiesListView citiesList



-- cityView


citiesListView : List City -> Html Msg
citiesListView cities =
    ul [] (List.map (\city -> li [] [ p [] [ text city.city ] ]) cities)


view : Model -> Html Msg
view model =
    div []
        [ headerView
        , contentView model
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
