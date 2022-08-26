module Flag exposing (Flag(..), decoder)

import Json.Decode as Decode exposing (Decoder)
import Location exposing (Location)


type Flag
    = AddItem Int
    | RemoveItem Int
    | AddCards (List Int)
    | RemoveCards (List Int)
    | ChangeLocation Location
    | Unknown


decoder : Decoder Flag
decoder =
    Decode.oneOf
        [ Decode.map AddItem (Decode.field "addItem" Decode.int)
        , Decode.map RemoveItem (Decode.field "removeItem" Decode.int)
        , Decode.map AddCards (Decode.field "addCards" (Decode.list Decode.int))
        , Decode.map RemoveCards (Decode.field "removeCards" (Decode.list Decode.int))
        , Decode.map ChangeLocation (Decode.field "changeLocation" Location.decoder)
        , Decode.succeed Unknown
        ]
