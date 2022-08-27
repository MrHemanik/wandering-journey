module Resources exposing (Resources, capResources, combine, deathMessage, decoder)

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


capResources : Resources -> Resources
capResources rs =
    { hunger = max 0 rs.hunger |> min 100
    , thirst = max 0 rs.thirst |> min 100
    , physicalHealth = max 0 rs.physicalHealth |> min 100
    , mentalHealth = max 0 rs.mentalHealth |> min 100
    , money = max 0 rs.money
    }


deathMessage : Resources -> String
deathMessage resources =
    if resources.hunger <= 0 then
        "You are starving, left with nothing to eat. Your stomach tightens painfully and you faint from the pain"

    else if resources.thirst <= 0 then
        "Your throat is dry and you find yourself getting weaker and weaker until you finally collapse. You die of thirst"

    else if resources.physicalHealth <= 0 then
        "Your whole body hurts from your numerous injuries. You try to find help but it is too late for you. Eventually, the pain overwhelms you, until you are no more"

    else if resources.mentalHealth <= 0 then
        "Your mind keeps playing tricks on you until you can no longer tell the difference between reality and imagination. You lost your sanity and yourself"

    else
        "You died for an unknown reason. Nobody in this world knows what happened to you"
