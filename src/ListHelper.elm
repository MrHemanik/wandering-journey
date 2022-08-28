module ListHelper exposing (addEntriesToList, idToObject, removeEntriesFromList)


removeEntriesFromList : List a -> List a -> List a
removeEntriesFromList list removeList =
    List.filter (\a -> not (List.member a removeList)) list


addEntriesToList : List comparable -> List comparable -> List comparable
addEntriesToList list addList =
    case addList of
        [] ->
            list

        x :: xs ->
            if List.member x list then
                addEntriesToList list xs

            else
                addEntriesToList (List.sort (x :: list)) xs


{-| Get an object with id 'id' from List Object

    idToObject 0 allCards == { id = 0, name = "Red crystal", description = "A mysterious crystal you bought from a strange merchant" }

    idToObject 0 allAchievements == { id = 0, name = "An endless wake", description = "Die for the first time" }

-}
idToObject : Int -> List { a | id : Int } -> Maybe { a | id : Int }
idToObject id idRecordList =
    case List.filter (\x -> x.id == id) idRecordList of
        [] ->
            Nothing

        x :: _ ->
            Just x
