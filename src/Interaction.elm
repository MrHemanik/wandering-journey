module Interaction exposing (Interaction(..), decoder)

import Json.Decode as Decode exposing (Decoder)


type Interaction
    = Conversation
    | Action


decoder : Decoder Interaction
decoder =
    Decode.string
        |> Decode.andThen
            (\interaction ->
                case interaction of
                    "Conversation" ->
                        Decode.succeed Conversation

                    "Action" ->
                        Decode.succeed Action

                    _ ->
                        Decode.fail "Invalid Interaction"
            )
