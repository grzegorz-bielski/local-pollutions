module Cities exposing (getPollutedCities, updateCities)

import Country as C
import Http
import Json.Decode as D
import Json.Decode.Pipeline as DP
import Model exposing (..)
import Msg exposing (..)



-- Update


updateCities : CitiesMsg -> CitiesModel
updateCities msg =
    case msg of
        GotCities res ->
            case res of
                Ok cities ->
                    Cities cities

                Err sth ->
                    Error


citiesDecoder : D.Decoder (List City)
citiesDecoder =
    D.field "results" (D.list cityDecoder)


cityDecoder : D.Decoder City
cityDecoder =
    D.succeed City
        |> DP.required "city" D.string
        |> DP.required "country" C.countryDecoder
        |> DP.required "locations" D.int
        |> DP.required "count" D.int


getPollutedCities : C.Country -> Cmd Msg
getPollutedCities country =
    let
        options =
            { expect =
                Http.expectJson (GotCitiesMsg << GotCities) citiesDecoder
            , url =
                "https://api.openaq.org/v1/cities?country="
                    ++ C.countryCode country
                    ++ "&order_by=count&sort=desc&limit=10"
            }
    in
    Http.get
        options



-- View
