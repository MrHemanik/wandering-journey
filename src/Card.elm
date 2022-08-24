module Card exposing (Card, decoder, getCardByIndex)

import Array
import DecodeHelper
import Interaction exposing (Interaction)
import Json.Decode as Decode exposing (Decoder)
import Location exposing (Location)
import Resources exposing (Resources)


type alias Card =
    { id : Int
    , possibleLocation : List Location
    , interaction : Interaction
    , mainText : String
    , decisionText1 : String
    , decisionText2 : String
    , followUpText1 : String
    , followUpText2 : String
    , resourceChange1 : Resources
    , resourceChange2 : Resources
    , newCards1 : List Int
    , newCards2 : List Int
    , removeCards1 : List Int
    , removeCards2 : List Int
    , locationChange1 : Location
    , locationChange2 : Location
    }


decoder : Decoder Card
decoder =
    Decode.succeed Card
        |> DecodeHelper.apply (Decode.field "id" Decode.int)
        |> DecodeHelper.apply (Decode.field "possibleLocation" (Decode.list Location.decoder))
        |> DecodeHelper.apply (Decode.field "interaction" Interaction.decoder)
        |> DecodeHelper.apply (Decode.field "mainText" Decode.string)
        |> DecodeHelper.apply (Decode.field "decisionText1" Decode.string)
        |> DecodeHelper.apply (Decode.field "decisionText2" Decode.string)
        |> DecodeHelper.apply (Decode.field "followUpText1" Decode.string)
        |> DecodeHelper.apply (Decode.field "followUpText2" Decode.string)
        |> DecodeHelper.apply (Decode.field "resourceChange1" Resources.decoder)
        |> DecodeHelper.apply (Decode.field "resourceChange2" Resources.decoder)
        |> DecodeHelper.apply (Decode.field "newCards1" (Decode.list Decode.int))
        |> DecodeHelper.apply (Decode.field "newCards2" (Decode.list Decode.int))
        |> DecodeHelper.apply (Decode.field "removeCards1" (Decode.list Decode.int))
        |> DecodeHelper.apply (Decode.field "removeCards2" (Decode.list Decode.int))
        |> DecodeHelper.apply (Decode.field "locationChange1" Location.decoder)
        |> DecodeHelper.apply (Decode.field "locationChange2" Location.decoder)


getCardByIndex : List Card -> Int -> Maybe Card
getCardByIndex list index =
    Array.get index (Array.fromList list)
