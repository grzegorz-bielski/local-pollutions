module Country exposing (Country(..), countryCode, countryDecoder, countryEnum, countryName, fromUserString)

import Json.Decode as D
import Json.Decode.Pipeline as DP


type Country
    = Germany
    | Poland
    | Spain
    | France


countryEnum =
    [ Germany, Poland, Spain, France ]


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


countryName : Country -> String
countryName country =
    case country of
        Germany ->
            "Germany"

        Poland ->
            "Poland"

        Spain ->
            "Spain"

        France ->
            "France"


fromUserString : String -> Maybe Country
fromUserString str =
    case String.toLower str of
        "poland" ->
            Just Poland

        "germany" ->
            Just Germany

        "spain" ->
            Just Spain

        "france" ->
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
