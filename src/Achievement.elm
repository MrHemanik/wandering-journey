module Achievement exposing (Achievement, achievementIdToAchievementUrl, checkDistance, decoder, unlockAchievement)

import Json.Decode as Decode exposing (Decoder)
import ListHelper
import Player exposing (Player)


type alias Achievement =
    { id : Int
    , name : String
    , description : String
    }


decoder : Decoder Achievement
decoder =
    Decode.map3 Achievement
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "description" Decode.string)


checkDistance : Int -> List Int
checkDistance score =
    [ 0 ]
        ++ (if score >= 100000 then
                [ 1, 2 ]

            else if score >= 10000 then
                [ 1 ]

            else
                []
           )


unlockAchievement : Int -> Player -> Player
unlockAchievement id player =
    { player | unlockedAchievements = ListHelper.addEntriesToList player.unlockedAchievements [ id ] }


achievementIdToAchievementUrl : Int -> String
achievementIdToAchievementUrl id =
    "src/img/achievements/" ++ String.fromInt id ++ ".png"
