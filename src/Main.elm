module Main exposing (init, main, update, view)

import Browser
import Cities exposing (updateCities)
import CitiesInfo exposing (citiesListView, updateCitiesInfo, updateCitiesInfoModel)
import Country exposing (..)
import Debug
import Dict exposing (Dict)
import Form exposing (selectView, updateForm)
import Html exposing (Html, div, form, h1, header, img, input, li, option, p, pre, select, span, text, ul)
import Html.Attributes exposing (class, placeholder, src, value)
import Html.Events exposing (custom, onBlur, onClick, onFocus, onInput, onSubmit)
import Http
import Json.Decode as D
import Json.Decode.Pipeline as DP
import Json.Encode as E
import List
import Model exposing (..)
import Msg exposing (..)
import Tuple



---- PROGRAM ----


init : E.Value -> ( Model, Cmd Msg )
init value =
    ( initialModel
    , Cmd.none
    )


initialModel : Model
initialModel =
    { cities = Empty
    , citiesInfo = Dict.empty
    , form =
        { selection = NotSelected
        , value = ""
        , isFocused = False
        }
    }


main : Program E.Value Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCitiesMsg subMsg ->
            ( { model | cities = updateCities subMsg }, Cmd.none )

        GotFormMsg subMsg ->
            updateForm subMsg model

        GotCitiesInfoMsg subMsg ->
            updateCitiesInfo subMsg model

        _ ->
            ( model, Cmd.none )



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


view : Model -> Html Msg
view model =
    div [ onClick <| GotFormMsg <| CloseDropdown, class "app-container" ]
        [ headerView
        , selectView model.form
        , contentView model
        ]
