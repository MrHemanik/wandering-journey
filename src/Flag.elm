module Flag exposing (Flag(..), decoder)

import Json.Decode as Decode exposing (Decoder)
import Location exposing (Location)


{-| States a modifier to our model.
Flags can be part of a decision and will be processed after a decision is made
or are part of a card and will be processed when a card is generated in form of CardFlag. This is useful for ConditionalDecisions

    AddItem 0

-}
type Flag
    = AddItem Int
    | RemoveItem Int
    | AddCards (List Int)
    | RemoveCards (List Int)
    | ChangeLocation Location
    | FollowUp Int
    | Unknown


decoder : Decoder Flag
decoder =
    Decode.oneOf
        [ Decode.map AddItem (Decode.field "addItem" Decode.int)
        , Decode.map RemoveItem (Decode.field "removeItem" Decode.int)
        , Decode.map AddCards (Decode.field "addCards" (Decode.list Decode.int))
        , Decode.map RemoveCards (Decode.field "removeCards" (Decode.list Decode.int))
        , Decode.map ChangeLocation (Decode.field "changeLocation" Location.decoder)
        , Decode.map FollowUp (Decode.field "followUp" Decode.int)
        , Decode.succeed Unknown
        ]
