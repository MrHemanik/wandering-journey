module Condition exposing (Condition(..), decoder, isCondition)

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


{-| Checks if a condition is true
-}
isCondition : Condition -> List Int -> Bool
isCondition condition activeItemsIndexes =
    case condition of
        OwnItem id ->
            List.member id activeItemsIndexes

        Unknown ->
            False
