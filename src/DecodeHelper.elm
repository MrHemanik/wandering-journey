module DecodeHelper exposing (apply, optional)

import Json.Decode as Decode exposing (Decoder)


apply : Decoder a -> Decoder (a -> b) -> Decoder b
apply =
    Decode.map2 (|>)


optional : Decoder a -> a -> Decoder a
optional decoder fallBack =
    Decode.oneOf
        [ decoder
        , Decode.succeed fallBack
        ]
