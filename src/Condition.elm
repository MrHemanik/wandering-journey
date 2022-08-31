module Condition exposing (Condition(..), decoder, isCondition)

import Json.Decode as Decode exposing (Decoder)


type Condition
    = OwnItem Int
    | OwnAllItems (List Int)
    | OwnTwoOfItems (List Int)
    | Unknown


decoder : Decoder Condition
decoder =
    Decode.oneOf
        [ Decode.map OwnItem (Decode.field "ownItem" Decode.int)
        , Decode.map OwnAllItems (Decode.field "ownAllItems" (Decode.list Decode.int))
        , Decode.map OwnTwoOfItems (Decode.field "ownTwoOfItems" (Decode.list Decode.int))
        , Decode.succeed Unknown
        ]


{-| Checks if a condition is true
-}
isCondition : Condition -> List Int -> Bool
isCondition condition activeItemsIndexes =
    case condition of
        OwnItem id ->
            List.member id activeItemsIndexes

        OwnAllItems idList ->
            List.all (\id -> List.member id activeItemsIndexes) idList

        OwnTwoOfItems idList ->
            List.length (List.filter (\isOwned -> isOwned == True) (List.map (\id -> List.member id activeItemsIndexes) idList)) >= 2

        Unknown ->
            False
