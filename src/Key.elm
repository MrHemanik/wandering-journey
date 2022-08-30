module Key exposing (Key(..), decoder)

import Choice exposing (Choice(..))
import Json.Decode as Decode exposing (Decoder)


type Key
    = ChoiceKey Choice
    | Restart
    | NumberKey Int
    | Controls
    | Achievements
    | UnknownKey


decoder : Decoder Key
decoder =
    let
        toKey string =
            case string of
                "ArrowLeft" ->
                    ChoiceKey Left

                "ArrowRight" ->
                    ChoiceKey Right

                "r" ->
                    Restart

                "1" ->
                    NumberKey 1

                "2" ->
                    NumberKey 2

                "3" ->
                    NumberKey 3

                "4" ->
                    NumberKey 4

                "5" ->
                    NumberKey 5

                "6" ->
                    NumberKey 6

                "7" ->
                    NumberKey 7

                "8" ->
                    NumberKey 8

                "9" ->
                    NumberKey 9

                "0" ->
                    NumberKey 0

                "c" ->
                    Controls

                "a" ->
                    Achievements

                _ ->
                    UnknownKey
    in
    Decode.map toKey (Decode.field "key" Decode.string)
