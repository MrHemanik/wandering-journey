module ListHelper exposing (addEntriesToList, addEntriesToListAndSort, idListToObjectList, idToObject, removeEntriesFromList)


removeEntriesFromList : List a -> List a -> List a
removeEntriesFromList list removeList =
    List.filter (\a -> not (List.member a removeList)) list


addEntriesToListAndSort : List comparable -> List comparable -> List comparable
addEntriesToListAndSort list addList =
    List.sort (addEntriesToList list addList)


addEntriesToList : List comparable -> List comparable -> List comparable
addEntriesToList list addList =
    list ++ removeEntriesFromList addList list


{-| Get an object with id 'id' from List Object

    idToObject 0 allCards == { id = 0, name = "Red crystal", description = "A mysterious crystal you bought from a strange merchant" }

    idToObject 0 allAchievements == { id = 0, name = "An endless wake", description = "Die for the first time" }

-}
idToObject : Int -> List { a | id : Int } -> Maybe { a | id : Int }
idToObject id idRecordList =
    List.head (List.filter (\x -> x.id == id) idRecordList)


{-| Get object corresponding to 'id' of idList from List Object.

    idListToObjectList [ 0, 2, 3 ] [ { id = 0 }, { id = 1 }, { id = 2 } ] == [ { id = 0 }, { id = 2 } ]

    idListToObjectList [ 3 ] [ { id = 0 }, { id = 1 }, { id = 2 } ] == []

-}
idListToObjectList : List Int -> List { a | id : Int } -> List { a | id : Int }
idListToObjectList idList idRecordList =
    List.filterMap identity <| List.map (\x -> idToObject x idRecordList) idList
