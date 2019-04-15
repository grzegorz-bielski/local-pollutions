module Form exposing (selectView, unWrapCountry, updateForm)

import Cities exposing (getPollutedCities)
import Country as C
import Html exposing (Html, div, form, h1, header, img, input, li, option, p, pre, select, span, text, ul)
import Html.Attributes exposing (class, placeholder, src, value)
import Html.Events exposing (custom, onBlur, onClick, onFocus, onInput, onSubmit)
import Json.Decode as D
import Model exposing (..)
import Msg exposing (..)


unWrapCountry : Maybe C.Country -> Model -> ( Model, Cmd Msg )
unWrapCountry maybeCountry model =
    case maybeCountry of
        Just country ->
            let
                oldFormModel =
                    .form model

                form =
                    { oldFormModel | selection = Selected country, isFocused = False, value = C.countryName country }
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

        OpenDropdown ->
            ( { model | form = { oldFormModel | isFocused = True } }, Cmd.none )

        CloseDropdown ->
            ( { model | form = { oldFormModel | isFocused = False } }, Cmd.none )



-- View


selectView : FormModel -> Html Msg
selectView model =
    let
        changeValue value =
            GotFormMsg <| InputChanged value

        openDropdown =
            GotFormMsg <| OpenDropdown

        setCountry val =
            GotFormMsg <| GotCountry <| C.fromUserString <| val

        onClickedOutside =
            custom "click"
                (D.succeed
                    { message = Noop
                    , stopPropagation = True
                    , preventDefault = False
                    }
                )

        countryListView =
            ul
                []
                (if model.isFocused then
                    C.countryEnum
                        |> List.map C.countryName
                        |> List.filter (\name -> String.startsWith (String.toLower model.value) (String.toLower name))
                        |> List.map (\name -> li [ onClick <| setCountry name ] [ text name ])

                 else
                    []
                )
    in
    div
        []
        [ form
            [ onSubmit <| setCountry model.value
            , onClickedOutside
            ]
            [ input
                [ placeholder "choose country..."
                , onInput changeValue
                , onFocus openDropdown
                , value model.value
                ]
                []
            , countryListView
            ]
        ]
