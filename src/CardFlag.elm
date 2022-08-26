module CardFlag exposing (..)

import Condition exposing (Condition)
import Decision exposing (Decision)
import Flag exposing (Flag)
import Json.Decode as Decode exposing (Decoder)



{- Flag isn't allowed to reference Decision so CardFlag is used when dealing with flags on a card -}


type CardFlag
    = ConditionalDecision Condition Bool Decision
    | DefaultFlag Flag


decoder : Decoder CardFlag
decoder =
    Decode.oneOf
        [ conditionalDecisionDecoder
        , Decode.map DefaultFlag Flag.decoder
        ]


conditionalDecisionDecoder : Decoder CardFlag
conditionalDecisionDecoder =
    Decode.map3 ConditionalDecision
        (Decode.at [ "conditionalDecision", "condition" ] Condition.decoder)
        (Decode.at [ "conditionalDecision", "overwriteSide" ] Decode.bool)
        (Decode.at [ "conditionalDecision", "decision" ] Decision.decoder)
