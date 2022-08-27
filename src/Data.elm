module Data exposing (Data(..), fromResult, handleError)

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


handleError : Error -> String
handleError err =
    case err of
        Field string error ->
            "Field Error:" ++ string

        Index int error ->
            "Index error:" ++ String.fromInt int

        OneOf errors ->
            "OneOf Error"

        _ ->
            "Failure Error:"
