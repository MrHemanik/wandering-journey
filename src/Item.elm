module Item exposing (Item, decoder)

import DecodeHelper
import Json.Decode as Decode exposing (Decoder)


type alias Item =
    { id : Int
    }


decoder : Decoder Item
decoder =
    Decode.succeed Item
        |> DecodeHelper.apply (Decode.field "id" Decode.int)
