module Player exposing (Player, decoder, encoder, unlockAchievement)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import ListHelper


type alias Player =
    { startingCards : List Int
    , unlockedAchievements : List Int
    , highscore : Int
    }


defaultPlayer =
    { startingCards = [], unlockedAchievements = [], highscore = 0 }


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


unlockAchievement : Int -> Player -> Player
unlockAchievement id player =
    { player | unlockedAchievements = ListHelper.addEntriesToList player.unlockedAchievements [ id ] }
