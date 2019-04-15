module Msg exposing (CitiesInfoMsg(..), CitiesMsg(..), FormMsg(..), Msg(..))

-- import CitiesInfo as CI

import Country as C
import Http
import Model exposing (City, WikiArticle)


type Msg
    = GotCitiesMsg CitiesMsg
    | GotCitiesInfoMsg CitiesInfoMsg
    | GotFormMsg FormMsg
    | Noop


type CitiesInfoMsg
    = CitiesDropdownToggled String
    | GotCitiesInfo String (Result Http.Error (List WikiArticle))


type FormMsg
    = GotCountry (Maybe C.Country)
    | OpenDropdown
    | CloseDropdown
    | InputChanged String


type CitiesMsg
    = GotCities (Result Http.Error (List City))
