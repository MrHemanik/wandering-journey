module Resources exposing (Resources, decoder)

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
