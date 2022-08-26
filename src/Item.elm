module Item exposing (Item, decoder, idToItem, itemToImageUrl)

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
    "src/img/items/" ++ String.fromInt id ++ ".png"


idToItem : Int -> List Item -> Maybe Item
idToItem id itemList =
    case itemList of
        [] ->
            Nothing

        x :: xs ->
            if id == x.id then
                Just x

            else
                idToItem id xs
