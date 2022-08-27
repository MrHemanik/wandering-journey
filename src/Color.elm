module Color exposing (..)

import Element exposing (rgb255, rgba)


black =
    rgb255 0x00 0x00 0x00


transBlackLight =
    rgba 0x00 0x00 0x00 0.4


transBlack =
    rgba 0x00 0x00 0x00 0.6


white =
    rgb255 0xFF 0xFF 0xFF


transWhite =
    rgba 0xFF 0xFF 0xFF 0.6


transWhiteHeavy =
    rgba 0xFF 0xFF 0xFF 0.8


red =
    rgb255 0xFF 0x00 0x00


green =
    rgb255 0x00 0xFF 0x00


yellow =
    rgb255 0xFF 0xF0 0x00
