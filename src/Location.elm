module Location exposing (Location(..), decoder, toText)

import Json.Decode as Decode exposing (Decoder)


type Location
    = Desert
    | Forest
    | City
    | Unknown



-- how to write decoder for custom types: https://stackoverflow.com/a/61857967 (took us long enough to find ...)


decoder : Decoder Location
decoder =
    Decode.string
        |> Decode.andThen
            (\loc ->
                case loc of
                    "Desert" ->
                        Decode.succeed Desert

                    "Forest" ->
                        Decode.succeed Forest

                    "City" ->
                        Decode.succeed City

                    _ ->
                        Decode.succeed Unknown
            )


toText : Location -> String
toText location =
    case location of
        Desert ->
            "Desert"

        Forest ->
            "Forest"

        City ->
            "City"

        _ ->
            "Unknown"
