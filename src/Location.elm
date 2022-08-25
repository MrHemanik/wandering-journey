module Location exposing (Location(..), decoder, toBackgroundImageUrl, toResourceDrain, toText)

import Json.Decode as Decode exposing (Decoder)
import Resources exposing (Resources)


type Location
    = Desert
    | Forest
    | City
    | None



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
                        Decode.succeed None
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


toBackgroundImageUrl : Location -> String
toBackgroundImageUrl location =
    "src/img/bg" ++ toText location ++ ".png"


toResourceDrain :
    Location
    -> Resources -- Resources that get removed after a card from that location is played
toResourceDrain location =
    case location of
        Desert ->
            { hunger = -2, thirst = -2, physicalHealth = 0, mentalHealth = -1, money = 0 }

        Forest ->
            { hunger = -1, thirst = -2, physicalHealth = 0, mentalHealth = -1, money = 0 }

        City ->
            { hunger = -1, thirst = -1, physicalHealth = 0, mentalHealth = 1, money = 0 }

        _ ->
            { hunger = 0, thirst = 0, physicalHealth = 0, mentalHealth = 0, money = 0 }
