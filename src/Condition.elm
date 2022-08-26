module Condition exposing (Condition(..), decoder)

import Json.Decode as Decode exposing (Decoder)


type Condition
    = OwnItem Int
    | Unknown


decoder : Decoder Condition
decoder =
    Decode.oneOf
        [ Decode.map OwnItem (Decode.field "ownItem" Decode.int)
        , Decode.succeed Unknown
        ]
