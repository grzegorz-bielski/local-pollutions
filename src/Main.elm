module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Debug
import Dict exposing (Dict)
import Html exposing (Html, div, form, h1, header, img, input, li, option, p, pre, select, span, text, ul)
import Html.Attributes exposing (placeholder, src, value)
import Html.Events exposing (onClick, onFocus, onInput, onSubmit)
import Http
import Json.Decode as D
import Json.Decode.Pipeline as DP
import List
import Tuple



---- MODEL ----


type CitiesModel
    = Error
    | Loading
    | Empty
    | Cities (List City)


type Info
    = InfoError
    | NoInfo
    | InfoLoading
    | Article WikiArticle


type alias CityInfoModel =
    { info : Info
    , dropdownToggled : Bool
    }


type alias CitiesInfoModel =
    Dict String CityInfoModel


type FormSelection
    = NotSelected
    | Selected Country


type alias FormModel =
    { selection : FormSelection
    , value : String
    , isFocused : Bool
    }


type alias Model =
    { cities : CitiesModel
    , citiesInfo : CitiesInfoModel
    , form : FormModel
    }


type Country
    = Germany
    | Poland
    | Spain
    | France


countryEnum =
    [ Germany, Poland, Spain, France ]


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

        Spain ->
            "ES"

        France ->
            "FR"


fromUserString : String -> Maybe Country
fromUserString str =
    case str of
        "Poland" ->
            Just Poland

        "Germany" ->
            Just Germany

        "Spain" ->
            Just Spain

        "France" ->
            Just France

        _ ->
            Nothing


countryDecoder : D.Decoder Country
countryDecoder =
    let
        decodeContry str =
            case str of
                "DE" ->
                    D.succeed Germany

                "PL" ->
                    D.succeed Poland

                "ES" ->
                    D.succeed Spain

                "FR" ->
                    D.succeed France

                other ->
                    D.fail <| "Unkown country: " ++ other
    in
    D.string |> D.andThen decodeContry


getPollutedCities : Country -> Cmd Msg
getPollutedCities country =
    let
        options =
            { expect =
                Http.expectJson (GotCitiesMsg << GotCities) citiesDecoder
            , url =
                "https://api.openaq.org/v1/cities?country="
                    ++ countryCode country
                    ++ "&order_by=count&sort=desc&limit=10"
            }
    in
    Http.get
        options


wikiURL : String
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


initialModel : Model
initialModel =
    { cities = Empty
    , citiesInfo = Dict.empty
    , form =
        { selection = Selected Germany
        , value = ""
        , isFocused = False
        }
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Cmd.none
    )



---- UPDATE ----


type CitiesMsg
    = GotCities (Result Http.Error (List City))


type CitiesInfoMsg
    = CitiesDropdownToggled String
    | GotCitiesInfo String (Result Http.Error (List WikiArticle))


type FormMsg
    = GotCountry (Maybe Country)
    | ToggleFocus
    | InputChanged String


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

                Err sth ->
                    Error


unWrapCountry : Maybe Country -> Model -> ( Model, Cmd Msg )
unWrapCountry maybeCountry model =
    case maybeCountry of
        Just country ->
            let
                oldFormModel =
                    .form model

                form =
                    { oldFormModel | selection = Selected country }
            in
            ( { model | form = form, cities = Loading }, getPollutedCities country )

        Nothing ->
            ( { model | cities = Empty }, Cmd.none )


updateForm : FormMsg -> Model -> ( Model, Cmd Msg )
updateForm msg model =
    let
        oldFormModel =
            .form model
    in
    case msg of
        GotCountry maybeCountry ->
            unWrapCountry maybeCountry model

        InputChanged value ->
            ( { model | form = { oldFormModel | value = value } }, Cmd.none )

        ToggleFocus ->
            ( { model | form = { oldFormModel | isFocused = not oldFormModel.isFocused } }, Cmd.none )


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
    -> CitiesInfoModel
    -> ( CitiesInfoModel, Cmd Msg )
updateCitiesInfo msg model =
    case msg of
        CitiesDropdownToggled location ->
            let
                oldCityModel =
                    findCityInfo location model

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
                    Dict.insert location newCityModel model

                cmd =
                    if shouldMakeRequest then
                        getCityInfo location

                    else
                        Cmd.none
            in
            ( newModel, cmd )

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
                    in
                    ( newModel location model, Cmd.none )

                Err _ ->
                    let
                        newModel =
                            updateCitiesInfoModel (\cityModel -> { cityModel | info = InfoError })
                    in
                    ( newModel location model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCitiesMsg subMsg ->
            ( { model | cities = updateCities subMsg }, Cmd.none )

        GotFormMsg subMsg ->
            updateForm subMsg model

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
        Empty ->
            text ""

        Error ->
            span [] [ text "Unable to load the asset" ]

        Loading ->
            span [] [ text "loading..." ]

        Cities citiesList ->
            citiesListView citiesList (.citiesInfo model)


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
    ul [] <| List.map matchCityToModel cities


selectView : FormModel -> Html Msg
selectView model =
    let
        handleChange value =
            GotFormMsg <| InputChanged value

        handleFocues _ =
            GotFormMsg <| ToggleFocus

        handleSubmit =
            GotFormMsg <| GotCountry <| fromUserString <| model.value
    in
    form [ onSubmit handleSubmit ]
        [ input [ placeholder "choose country...", onInput handleChange, value model.value ] []
        ]


view : Model -> Html Msg
view model =
    div []
        [ headerView
        , selectView model.form
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
