module Flag exposing (..)

import DecodeHelper
import Json.Decode as Decode exposing (Decoder)


type alias Flag =
    { flag : String
    , content : String
    }


decoder : Decoder Flag
decoder =
    Decode.succeed Flag
        |> DecodeHelper.apply (Decode.field "flag" Decode.string)
        |> DecodeHelper.apply (Decode.field "content" Decode.string)
