module Decision exposing (Decision, decoder)

import DecodeHelper
import Flag exposing (Flag)
import Json.Decode as Decode exposing (Decoder)
import Resources exposing (Resources)


type alias Decision =
    { choiceText : String
    , pickedText : String
    , resourceChange : Resources
    , newCards : List Int
    , removeCards : List Int
    , flags : List Flag
    }


decoder : Decoder Decision
decoder =
    Decode.succeed Decision
        |> DecodeHelper.apply (Decode.field "choiceText" Decode.string)
        |> DecodeHelper.apply (Decode.field "pickedText" Decode.string)
        |> DecodeHelper.apply (Decode.field "resourceChange" Resources.decoder)
        |> DecodeHelper.apply (Decode.field "newCards" (Decode.list Decode.int))
        |> DecodeHelper.apply (Decode.field "removeCards" (Decode.list Decode.int))
        |> DecodeHelper.apply (Decode.field "flags" (Decode.list Flag.decoder))
