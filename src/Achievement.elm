module Achievement exposing (Achievement, achievementIdToAchievementUrl, checkDistance, unlockAchievement)

import ListHelper
import Player exposing (Player)


type alias Achievement =
    { id : Int
    , name : String
    , description : String
    }


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
