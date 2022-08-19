module Main exposing (main)

import Browser
import Browser.Events
import Card exposing (Card)
import DecodeHelper
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (height, src, width)
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
    String


type Model
    = GameOver DeathCause (Maybe Game) Highscore
    | Running Choice (Maybe Game) Highscore



-- Resources you need to manage.


type Msg
    = Key Key
    | NewCard Int


type alias Game =
    { resources : Resources, allCards : List Card, possibleCardIndexes : List Int, currentDeck : List Card, location : Location, card : Maybe Card }


type alias Data =
    { allCards : List Card, startingCardIndexes : List Int }


startingResources =
    { hunger = 100, thirst = 100, physicalHealth = 100, mentalHealth = 100, money = 100 }


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


dataDecoder : Decoder Data
dataDecoder =
    Decode.succeed Data
        |> DecodeHelper.apply (Decode.field "cards" (Decode.list Card.decoder))
        |> DecodeHelper.apply (Decode.field "startingCards" (Decode.list Decode.int))



-- TODO Everything below
-- TODO: read json into elm https://gist.github.com/benkoshy/d0dcd2b09f8fcc65a90b56a33dcf1465
---- Visuals ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "src/Img1.png", width 300, height 300 ] []
        , div []
            [ case model of
                Running _ _ num ->
                    text num

                GameOver _ _ _ ->
                    text ""
            ]
        ]



---- Update ----


update : Msg -> Model -> ( Model, Cmd Msg )
update command model =
    ( model, Cmd.none )



---- Default functions ----


init : String -> ( Model, Cmd Msg )
init flags =
    ( Running Left Nothing flags, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map Key (Browser.Events.onKeyDown keyDecoder)


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }
