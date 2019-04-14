module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, h1, header, img, li, p, pre, span, text, ul)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Decode.Pipeline as DP
import List
import Tuple



---- MODEL ----


type CitiesModel
    = Error
    | Loading
    | Cities (List City)


type CityInfoModel
    = InfoError
    | NoInfo
    | InfoLoading
    | Article WikiArticle


type alias CitiesInfoModel =
    Dict String CityInfoModel


type FormModel
    = NotSelected
    | Selected Country


type alias Model =
    { cities : CitiesModel
    , citiesInfo : CitiesInfoModel
    , form : FormModel
    }


type Country
    = Germany
    | Poland


type alias City =
    { city : String
    , country : Country
    , locations : Int
    , count : Int
    }


type alias WikiArticle =
    { title : String
    , extract : String
    }


wikiDecoder : D.Decoder (List WikiArticle)
wikiDecoder =
    (D.dict wikiArticleDecoder |> D.field "pages" |> D.field "query")
        |> D.andThen (\d -> D.succeed <| List.map Tuple.second (Dict.toList d))


wikiArticleDecoder : D.Decoder WikiArticle
wikiArticleDecoder =
    D.succeed WikiArticle
        |> DP.required "title" D.string
        |> DP.required "extract" D.string


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
    Http.get { url = url, expect = Http.expectJson (GotCitiesMsg << GotCities) citiesDecoder }


getCityInfo : String -> Cmd Msg
getCityInfo location =
    let
        url =
            "https://en.wikipedia.org/w/api.php?format=json&action=query&prop=extracts&exintro&explaintext&redirects=1&origin=*&titles=" ++ location
    in
    Http.get { url = url, expect = Http.expectJson (GotCitiesInfoMsg << GotCitiesInfo location) wikiDecoder }


initialModel : Model
initialModel =
    { cities = Loading
    , citiesInfo = Dict.empty
    , form = Selected Germany
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , getPollutedCities Germany
    )



---- UPDATE ----


type CitiesMsg
    = GotCities (Result Http.Error (List City))


type CitiesInfoMsg
    = CitiesInfoRequested String
    | GotCitiesInfo String (Result Http.Error (List WikiArticle))


type FormMsg
    = GotCountry Country


type Msg
    = GotCitiesMsg CitiesMsg
    | GotCitiesInfoMsg CitiesInfoMsg
    | GotFormMsg FormMsg


updateCities : CitiesMsg -> CitiesModel
updateCities msg =
    case msg of
        GotCities res ->
            case res of
                Ok cities ->
                    Cities cities

                Err _ ->
                    Error


updateForm : FormMsg -> FormModel
updateForm msg =
    case msg of
        GotCountry country ->
            Selected country


getCityInfoModel : List WikiArticle -> CityInfoModel
getCityInfoModel cityInfos =
    case List.head cityInfos of
        Just cityInfo ->
            Article cityInfo

        Nothing ->
            InfoError


updateCitiesInfo : CitiesInfoMsg -> CitiesInfoModel -> ( CitiesInfoModel, Cmd Msg )
updateCitiesInfo msg model =
    case msg of
        CitiesInfoRequested location ->
            ( Dict.insert location InfoLoading model, getCityInfo location )

        GotCitiesInfo location res ->
            let
                updater =
                    case res of
                        Ok cityInfos ->
                            Dict.insert location (getCityInfoModel cityInfos)

                        Err _ ->
                            Dict.insert location InfoError
            in
            ( updater model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCitiesMsg subMsg ->
            ( { model | cities = updateCities subMsg }, Cmd.none )

        GotFormMsg subMsg ->
            ( { model | form = updateForm subMsg }, Cmd.none )

        GotCitiesInfoMsg subMsg ->
            let
                ( citiesInfoUpdate, cmd ) =
                    updateCitiesInfo subMsg model.citiesInfo
            in
            ( { model | citiesInfo = citiesInfoUpdate }, cmd )



---- VIEW ----


headerView : Html Msg
headerView =
    header []
        [ img [ src "/logo.svg" ] [] ]


contentView : Model -> Html Msg
contentView model =
    case .cities model of
        Error ->
            span [] [ text "Unable to load the asset" ]

        Loading ->
            span [] [ text "loading..." ]

        Cities citiesList ->
            citiesListView citiesList (.citiesInfo model)


cityInfoView : CityInfoModel -> Html Msg
cityInfoView cityInfoModel =
    case cityInfoModel of
        NoInfo ->
            text "No info about this location"

        InfoLoading ->
            text "Loading..."

        InfoError ->
            text "Couldn't fetch info"

        Article wikiArticle ->
            text (.extract wikiArticle)


cityView : CityInfoModel -> City -> Html Msg
cityView cityInfoModel city =
    let
        cityName =
            .city city

        handleClick =
            GotCitiesInfoMsg <| CitiesInfoRequested cityName
    in
    li []
        [ div []
            [ p [ onClick handleClick ] [ text cityName ]
            , p [] [ cityInfoView cityInfoModel ]
            ]
        ]


citiesListView : List City -> CitiesInfoModel -> Html Msg
citiesListView cities citiesInfoModel =
    let
        matchCityToModel city =
            case Dict.get (.city city) citiesInfoModel of
                Just cityInfoModel ->
                    cityView cityInfoModel city

                Nothing ->
                    cityView NoInfo city
    in
    ul [] <| List.map matchCityToModel cities


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
