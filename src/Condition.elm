module Condition exposing (Condition(..), decoder, isCondition)

import Json.Decode as Decode exposing (Decoder)


type Condition
    = OwnItem Int
    | OwnAllItems (List Int)
    | OwnXItems Int (List Int)
    | Unknown


decoder : Decoder Condition
decoder =
    Decode.oneOf
        [ Decode.map OwnItem (Decode.field "ownItem" Decode.int)
        , Decode.map OwnAllItems (Decode.field "ownAllItems" (Decode.list Decode.int))
        , Decode.map2 OwnXItems (Decode.at [ "ownXItems", "amountOwned" ] Decode.int) (Decode.at [ "ownXItems", "list" ] (Decode.list Decode.int))
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

        OwnXItems amount idList ->
            List.length (List.filter (\isOwned -> isOwned == True) (List.map (\id -> List.member id activeItemsIndexes) idList)) >= amount

        Unknown ->
            False
