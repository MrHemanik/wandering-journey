module Data exposing (Data(..), fromResult)

import Json.Decode exposing (Error(..))


type Data value
    = Failure Error
    | Success value


fromResult : Result Error a -> Data a
fromResult result =
    case result of
        Ok value ->
            Success value

        Err e ->
            Failure e
