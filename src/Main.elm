module Main exposing (main)

import Browser
import Browser.Events
import Card exposing (Card)
import Data
import DecodeHelper
import Html exposing (Html, div, text)
import Json.Decode as Decode exposing (Decoder)
import Location exposing (Location)
import Random
import Resources exposing (Resources)


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
    = GameOver Game Highscore
    | Running Choice Game Highscore



-- Resources you need to manage.


type Msg
    = Key Key
    | NewCard Int


type alias Game =
    { resources : Resources, allCards : List Card, possibleCardIndexes : List Int, currentDeck : Maybe (List Card), location : Location, card : Maybe Card }


type alias JsonData =
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


dataDecoder : Decoder JsonData
dataDecoder =
    Decode.succeed JsonData
        |> DecodeHelper.apply (Decode.field "cards" (Decode.list Card.decoder))
        |> DecodeHelper.apply (Decode.field "startingCards" (Decode.list Decode.int))



-- TODO Everything below
-- TODO: read json into elm https://gist.github.com/benkoshy/d0dcd2b09f8fcc65a90b56a33dcf1465
---- Visuals ----


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ case model of
                GameOver game _ ->
                    text (viewDeathMessage game.resources)

                Running _ game _ ->
                    text (viewResources game.resources)
            ]
        , div []
            [ case model of
                Running _ game _ ->
                    div []
                        [ div []
                            [ let
                                a =
                                    Card.getCardByIndex game.allCards 0
                              in
                              case a of
                                Just c ->
                                    text c.mainText

                                _ ->
                                    text ""
                            ]
                        ]

                GameOver _ _ ->
                    text ""
            ]
        ]


newPossibleCardIndex : Game -> Cmd Msg
newPossibleCardIndex game =
    let
        generator listSize =
            Random.int 0 (listSize - 1)
    in
    Random.generate NewCard (generator (List.length game.possibleCardIndexes))


viewResources : Resources -> String
viewResources resources =
    "Hunger: "
        ++ String.fromInt resources.hunger
        ++ " Thirst: "
        ++ String.fromInt resources.thirst
        ++ " Physical Health: "
        ++ String.fromInt resources.physicalHealth
        ++ " Mental Health: "
        ++ String.fromInt resources.mentalHealth
        ++ " Money: "
        ++ String.fromInt resources.money


viewDeathMessage : Resources -> String
viewDeathMessage resources =
    if resources.hunger <= 0 then
        "Died of starvation"

    else if resources.thirst <= 0 then
        "Died of thirst"

    else if resources.physicalHealth <= 0 then
        "Died due to injuries"

    else if resources.mentalHealth <= 0 then
        "Died due to mental health"

    else if resources.money <= 0 then
        "No money left"

    else
        "Died of an unknown cause"



---- Update ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        GameOver game highscore ->
            ( model, Cmd.none )

        Running choice game highscore ->
            case msg of
                NewCard int ->
                    ( Running choice { game | card = Card.getCardByIndex game.allCards int } highscore, newPossibleCardIndex game )

                Key key ->
                    ( model, Cmd.none )



---- Default functions ----


init : String -> ( Model, Cmd Msg )
init flags =
    let
        dataResponse =
            Decode.decodeString dataDecoder flags

        data =
            Data.fromResult dataResponse
    in
    case data of
        Data.Success value ->
            ( Running Left { resources = startingResources, allCards = value.allCards, possibleCardIndexes = value.startingCardIndexes, currentDeck = Nothing, location = Location.City, card = Nothing } flags, Cmd.none )

        Data.Loading ->
            ( Running Left { resources = startingResources, allCards = [], possibleCardIndexes = [], currentDeck = Nothing, location = Location.City, card = Nothing } "Loading", Cmd.none )

        Data.Failure e ->
            ( Running Left { resources = startingResources, allCards = [], possibleCardIndexes = [], currentDeck = Nothing, location = Location.City, card = Nothing } (flags ++ Decode.errorToString e), Cmd.none )


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
