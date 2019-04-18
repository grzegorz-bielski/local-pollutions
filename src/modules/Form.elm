port module Form exposing (selectView, setCountry, unWrapCountry, updateForm)

import Cities exposing (getPollutedCities)
import Country as C
import Debug
import Html exposing (Html, div, form, h1, header, img, input, li, option, p, pre, select, span, text, ul)
import Html.Attributes exposing (class, placeholder, src, value)
import Html.Events exposing (custom, onBlur, onClick, onFocus, onInput, onSubmit)
import Json.Decode as D
import Json.Encode as E
import Model exposing (..)
import Msg exposing (..)


port store : String -> Cmd msg


unWrapCountry : Maybe C.Country -> Model -> ( Model, Cmd Msg )
unWrapCountry maybeCountry model =
    case maybeCountry of
        Just country ->
            let
                countryNameStringified =
                    C.countryName country

                oldFormModel =
                    .form model

                form =
                    { oldFormModel
                        | selection = Selected country
                        , isFocused = False
                        , value = countryNameStringified
                    }

                newModel =
                    { model | form = form, cities = Loading }

                cmds =
                    Cmd.batch
                        [ getPollutedCities country
                        , store countryNameStringified
                        ]
            in
            ( newModel, cmds )

        Nothing ->
            ( { model | cities = Empty }, Cmd.none )


updateForm : FormMsg -> Model -> ( Model, Cmd Msg )
updateForm msg model =
    let
        oldFormModel =
            .form model

        setValue value =
            { oldFormModel | value = value }

        setFocus value =
            { oldFormModel | isFocused = value }

        update form =
            ( { model | form = form }, Cmd.none )
    in
    case msg of
        GotCountry maybeCountry ->
            Debug.log "called" (unWrapCountry maybeCountry model)

        InputChanged value ->
            update <| setValue value

        OpenDropdown ->
            update <| setFocus True

        CloseDropdown ->
            update <| setFocus False



-- View


setCountry : String -> Msg
setCountry =
    GotFormMsg << GotCountry << C.fromUserString


selectView : FormModel -> Html Msg
selectView model =
    let
        changeValue value =
            GotFormMsg <| InputChanged value

        openDropdown =
            GotFormMsg <| OpenDropdown

        onClickedOutside =
            custom "click"
                (D.succeed
                    { message = Noop
                    , stopPropagation = True
                    , preventDefault = False
                    }
                )

        notContainingTheString name =
            String.startsWith (String.toLower model.value) (String.toLower name)

        contryListItem name =
            li [ onClick <| setCountry name ] [ text name ]

        countryListView =
            ul
                []
                (if model.isFocused then
                    C.countryEnum
                        |> List.map C.countryName
                        |> List.filter notContainingTheString
                        |> List.map contryListItem

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
