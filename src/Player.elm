module Player exposing (Player, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Player =
    { startingCards : List Int
    , unlockedAchievements : List Int
    , highscore : Int
    }


decoder : Decoder Player
decoder =
    Decode.map3 Player
        (Decode.field "startingCards" <| Decode.list Decode.int)
        (Decode.field "unlockedAchievements" <| Decode.list Decode.int)
        (Decode.field "highscore" Decode.int)


encoder : Player -> Encode.Value
encoder player =
    Encode.object
        [ ( "startingCards", Encode.list Encode.int player.startingCards )
        , ( "unlockedAchievements", Encode.list Encode.int player.unlockedAchievements )
        , ( "highscore", Encode.int player.highscore )
        ]
