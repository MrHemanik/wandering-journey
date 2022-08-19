module DecodeHelper exposing (apply)

import Json.Decode as Decode exposing (Decoder)


apply : Decoder a -> Decoder (a -> b) -> Decoder b
apply =
    Decode.map2 (|>)
