module Card exposing (Card, decoder, getCardByIndex)

import Array
import Decision exposing (Decision)
import DecodeHelper
import Interaction exposing (Interaction)
import Json.Decode as Decode exposing (Decoder)
import Location exposing (Location)


type alias Card =
    { id : Int
    , possibleLocation : List Location
    , interaction : Interaction
    , mainText : String
    , decisionLeft : Decision
    , decisionRight : Decision
    }


decoder : Decoder Card
decoder =
    Decode.succeed Card
        |> DecodeHelper.apply (Decode.field "id" Decode.int)
        |> DecodeHelper.apply (Decode.field "possibleLocation" (Decode.list Location.decoder))
        |> DecodeHelper.apply (Decode.field "interaction" Interaction.decoder)
        |> DecodeHelper.apply (Decode.field "mainText" Decode.string)
        |> DecodeHelper.apply (Decode.field "decisionLeft" Decision.decoder)
        |> DecodeHelper.apply (Decode.field "decisionRight" Decision.decoder)


getCardByIndex : List Card -> Int -> Maybe Card
getCardByIndex list index =
    Array.get index (Array.fromList list)
