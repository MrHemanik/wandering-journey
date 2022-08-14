module Main exposing (main)

import Browser
import Html exposing (Html, div)


type Model
    = Name String


type Msg
    = Tick
    | ChangeName String



---- Visuals ----


view : Model -> Html Msg
view model =
    div [] []



---- Update ----


update : Msg -> Model -> ( Model, Cmd Msg )
update command model =
    ( model, Cmd.none )



---- Default functions ----


init : String -> ( Model, Cmd Msg )
init playerName =
    ( Name "banana", Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init ""
        , subscriptions = subscriptions
        , view = view
        , update = update
        }
