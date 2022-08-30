module Condition exposing (Condition(..), decoder, isCondition)

import Json.Decode as Decode exposing (Decoder)


type Condition
    = OwnItem Int
    | OwnItems (List Int)
    | Unknown


decoder : Decoder Condition
decoder =
    Decode.oneOf
        [ Decode.map OwnItem (Decode.field "ownItem" Decode.int)
        , Decode.map OwnItems (Decode.field "ownItems" (Decode.list Decode.int))
        , Decode.succeed Unknown
        ]


{-| Checks if a condition is true
-}
isCondition : Condition -> List Int -> Bool
isCondition condition activeItemsIndexes =
    case condition of
        OwnItem id ->
            List.member id activeItemsIndexes

        OwnItems idList ->
            List.all (\id -> List.member id activeItemsIndexes) idList

        Unknown ->
            False
