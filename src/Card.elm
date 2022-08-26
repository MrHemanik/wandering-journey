module Card exposing (Card, decoder, getCardByIndex)

import Array
import CardFlag exposing (CardFlag)
import Decision exposing (Decision)
import DecodeHelper as Dh
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
    , flags : List CardFlag
    }


decoder : Decoder Card
decoder =
    Decode.succeed Card
        |> Dh.apply (Decode.field "id" Decode.int)
        |> Dh.apply (Decode.field "possibleLocation" (Decode.list Location.decoder))
        |> Dh.apply (Decode.field "interaction" Interaction.decoder)
        |> Dh.apply (Decode.field "mainText" Decode.string)
        |> Dh.apply (Decode.field "decisionLeft" Decision.decoder)
        |> Dh.apply (Decode.field "decisionRight" Decision.decoder)
        |> Dh.apply (Dh.optional (Decode.field "flags" (Decode.list CardFlag.decoder)) [])


getCardByIndex : List Card -> Int -> Maybe Card
getCardByIndex list index =
    Array.get index (Array.fromList list)
