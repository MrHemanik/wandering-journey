module ListHelper exposing (addEntriesToList, removeEntriesFromList)


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
