module Resources exposing (Resources, combine, decoder)

import Json.Decode as Decode exposing (Decoder)


type alias Resources =
    { hunger : Int, thirst : Int, physicalHealth : Int, mentalHealth : Int, money : Int }


decoder : Decoder Resources
decoder =
    Decode.map5 Resources
        (Decode.field "hunger" Decode.int)
        (Decode.field "thirst" Decode.int)
        (Decode.field "physicalHealth" Decode.int)
        (Decode.field "mentalHealth" Decode.int)
        (Decode.field "money" Decode.int)


combine : Resources -> Resources -> Resources
combine resources1 resources2 =
    { hunger = resources1.hunger + resources2.hunger
    , thirst = resources1.thirst + resources2.thirst
    , physicalHealth = resources1.physicalHealth + resources2.physicalHealth
    , mentalHealth = resources1.mentalHealth + resources2.mentalHealth
    , money = resources1.money + resources2.money
    }
