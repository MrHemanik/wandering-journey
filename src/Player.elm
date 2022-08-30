module Player exposing (Player, decoder, encoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Player =
    { unlockedAchievements : List Int
    , highscore : Int
    }


decoder : Decoder Player
decoder =
    Decode.map2 Player
        (Decode.field "unlockedAchievements" <| Decode.list Decode.int)
        (Decode.field "highscore" Decode.int)


encoder : Player -> Encode.Value
encoder player =
    Encode.object
        [ ( "unlockedAchievements", Encode.list Encode.int player.unlockedAchievements )
        , ( "highscore", Encode.int player.highscore )
        ]
