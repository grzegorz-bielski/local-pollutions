module CitiesInfo exposing (citiesListView, updateCitiesInfo, updateCitiesInfoModel)

import Dict exposing (Dict)
import Html exposing (Html, div, form, h1, header, img, input, li, option, p, pre, select, span, text, ul)
import Html.Attributes exposing (class, placeholder, src, value)
import Html.Events exposing (custom, onBlur, onClick, onFocus, onInput, onSubmit)
import Http
import Json.Decode as D
import Json.Decode.Pipeline as DP
import Model exposing (CitiesInfoModel, City, CityInfoModel, Info(..), Model, WikiArticle)
import Msg exposing (..)



-- Update


updateCitiesInfoModel :
    (CityInfoModel -> CityInfoModel)
    -> String
    -> CitiesInfoModel
    -> CitiesInfoModel
updateCitiesInfoModel updater location model =
    let
        oldCityModel =
            findCityInfo location model

        newCityModel =
            updater oldCityModel
    in
    Dict.insert location newCityModel model


updateCitiesInfo :
    CitiesInfoMsg
    -> Model
    -> ( Model, Cmd Msg )
updateCitiesInfo msg model =
    case msg of
        CitiesDropdownToggled location ->
            let
                oldCityModel =
                    findCityInfo location model.citiesInfo

                shouldMakeRequest =
                    case .info oldCityModel of
                        NoInfo ->
                            True

                        _ ->
                            False

                shouldToggleDropdown =
                    not (.dropdownToggled oldCityModel)

                info =
                    if shouldMakeRequest then
                        InfoLoading

                    else
                        .info oldCityModel

                newCityModel =
                    { oldCityModel
                        | dropdownToggled = shouldToggleDropdown
                        , info = info
                    }

                newModel =
                    Dict.insert location newCityModel model.citiesInfo

                cmd =
                    if shouldMakeRequest then
                        getCityInfo location

                    else
                        Cmd.none
            in
            ( { model | citiesInfo = newModel }, cmd )

        GotCitiesInfo location res ->
            case res of
                Ok cityInfos ->
                    let
                        info =
                            case List.head cityInfos of
                                Just cityInfo ->
                                    Article cityInfo

                                Nothing ->
                                    InfoError

                        newModel =
                            updateCitiesInfoModel
                                (\cityModel ->
                                    { cityModel | info = info }
                                )
                                location
                                model.citiesInfo
                    in
                    ( { model | citiesInfo = newModel }, Cmd.none )

                Err _ ->
                    let
                        newModel =
                            updateCitiesInfoModel (\cityModel -> { cityModel | info = InfoError }) location model.citiesInfo
                    in
                    ( { model | citiesInfo = newModel }, Cmd.none )


wikiDecoder : D.Decoder (List WikiArticle)
wikiDecoder =
    (D.dict wikiArticleDecoder |> D.field "pages" |> D.field "query")
        |> D.andThen (\d -> D.succeed <| List.map Tuple.second (Dict.toList d))


wikiArticleDecoder : D.Decoder WikiArticle
wikiArticleDecoder =
    D.succeed WikiArticle
        |> DP.required "title" D.string
        |> DP.required "extract" D.string


wikiURL =
    "https://en.wikipedia.org/w/api.php?format=json&action=query&prop=extracts&exintro&explaintext&redirects=1&origin=*&titles="


getCityInfo : String -> Cmd Msg
getCityInfo location =
    let
        options =
            { expect =
                Http.expectJson (GotCitiesInfoMsg << GotCitiesInfo location) wikiDecoder
            , url =
                wikiURL ++ location
            }
    in
    Http.get
        options



--- View


cityInfoView : CityInfoModel -> Html Msg
cityInfoView infoModel =
    if .dropdownToggled infoModel then
        case .info infoModel of
            NoInfo ->
                text "No info about this location"

            InfoLoading ->
                text "Loading..."

            InfoError ->
                text "Couldn't fetch info"

            Article wikiArticle ->
                text (.extract wikiArticle)

    else
        text ""


cityView : City -> CityInfoModel -> Html Msg
cityView city cityInfoModel =
    let
        cityName =
            .city city

        handleClick =
            GotCitiesInfoMsg <| CitiesDropdownToggled cityName
    in
    li []
        [ div []
            [ p [ onClick handleClick ] [ text cityName ]
            , p [] [ cityInfoView cityInfoModel ]
            ]
        ]


findCityInfo : String -> CitiesInfoModel -> CityInfoModel
findCityInfo location citiesInfoModel =
    case Dict.get location citiesInfoModel of
        Just cityInfoModel ->
            cityInfoModel

        Nothing ->
            { info = NoInfo, dropdownToggled = False }


citiesListView : List City -> CitiesInfoModel -> Html Msg
citiesListView cities citiesInfoModel =
    let
        matchCityToModel city =
            cityView city <| findCityInfo (.city city) citiesInfoModel
    in
    ul [ class "country-list" ] <| List.map matchCityToModel cities
