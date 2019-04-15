module Model exposing (CitiesInfoModel, CitiesModel(..), City, CityInfoModel, FormModel, FormSelection(..), Info(..), Model, WikiArticle, initialModel)

import Country as C
import Dict exposing (Dict)


type alias Model =
    { cities : CitiesModel
    , citiesInfo : CitiesInfoModel
    , form : FormModel
    }


initialModel : Model
initialModel =
    { cities = Empty
    , citiesInfo = Dict.empty
    , form =
        { selection = Selected C.Germany
        , value = ""
        , isFocused = False
        }
    }



-- cities


type CitiesModel
    = Error
    | Loading
    | Empty
    | Cities (List City)


type alias City =
    { city : String
    , country : C.Country
    , locations : Int
    , count : Int
    }



-- cities info


type alias CityInfoModel =
    { info : Info
    , dropdownToggled : Bool
    }


type alias WikiArticle =
    { title : String
    , extract : String
    }


type Info
    = InfoError
    | NoInfo
    | InfoLoading
    | Article WikiArticle


type alias CitiesInfoModel =
    Dict String CityInfoModel



-- form


type FormSelection
    = NotSelected
    | Selected C.Country


type alias FormModel =
    { selection : FormSelection
    , value : String
    , isFocused : Bool
    }
