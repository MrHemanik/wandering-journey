module Main exposing (main)

import Browser
import Browser.Events
import Card exposing (Card)
import Html exposing (Html, div)
import Json.Decode as Decode exposing (Decoder)
import Location exposing (Location)
import Resources exposing (Resources)



-- Maybe unnecessary because it's portrayed in Game (Resource is 0 on deathreason)


type DeathCause
    = Hunger
    | Thirst
    | PhysicalHealth
    | MentalHealth
    | Money


type Choice
    = Left
    | Right


type Key
    = ChoiceKey Choice
    | R
    | UnknownKey


type alias Highscore =
    Int


type Model
    = GameOver DeathCause Game Highscore
    | Running Choice Game Highscore



-- Resources you need to manage.


type Msg
    = Key Key
    | NewCard Int


type alias Game =
    { resources : Resources, allCards : List Card, possibleCardIndexes : List Int, currentDeck : List Card, location : Location, card : Maybe Card }


keyDecoder : Decoder Key
keyDecoder =
    let
        toKey string =
            case string of
                "ArrowLeft" ->
                    ChoiceKey Left

                "ArrowRight" ->
                    ChoiceKey Right

                "r" ->
                    R

                _ ->
                    UnknownKey
    in
    Decode.map toKey (Decode.field "key" Decode.string)



-- TODO Everything below
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
    ( Running, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map Key (Browser.Events.onKeyDown keyDecoder)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init ""
        , subscriptions = subscriptions
        , view = view
        , update = update
        }
