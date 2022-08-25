module Item exposing (Item, decoder)

import DecodeHelper
import Json.Decode as Decode exposing (Decoder)


type alias Item =
    { id : Int
    , name : String
    , description : String
    }


decoder : Decoder Item
decoder =
    Decode.succeed Item
        |> DecodeHelper.apply (Decode.field "id" Decode.int)
        |> DecodeHelper.apply (Decode.field "name" Decode.string)
        |> DecodeHelper.apply (Decode.field "description" Decode.string)


itemToImageUrl : Int -> String
itemToImageUrl id =
    "src/img/items/" ++ String.fromInt id
