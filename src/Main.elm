module Main exposing (main)

import Browser
import Browser.Events
import Card exposing (Card)
import Data
import DecodeHelper
import Element exposing (Element, alpha, centerX, centerY, clip, el, fill, height, image, inFront, layout, none, padding, px, rgb255, spaceEvenly, spacing, width)
import Element.Background as Background exposing (color)
import Element.Input
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Location exposing (Location)
import Random
import Resources exposing (Resources)


type Choice
    = Left
    | Right
    | None


type Key
    = ChoiceKey Choice
    | R
    | UnknownKey


type alias Highscore =
    Int


type Model
    = GameOver Game Highscore
    | Running Choice Game Highscore



-- Resources you need to manage.


type Msg
    = Key Key
    | NewCard Int
    | GenerateNewCard


type alias Game =
    { resources : Resources, allCards : List Card, unlockedCardIndexes : List Int, currentCards : List Card, location : Location, card : Maybe Card }


type alias JsonData =
    { allCards : List Card, startingCardIndexes : List Int }



---- Preset Variables ----


startingResources =
    { hunger = 100, thirst = 100, physicalHealth = 100, mentalHealth = 100, money = 100 }


emptyResources =
    { hunger = 0, thirst = 0, physicalHealth = 0, mentalHealth = 0, money = 0 }


startingLocation =
    Location.City



---- Decoder ----


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
    let
        game =
            case model of
                GameOver gameState _ ->
                    gameState

                Running _ gameState _ ->
                    gameState
    in
    viewBackground game.location <|
        el [ centerX, centerY, Background.color (rgb255 0xFF 0xFF 0xFF) ] <|
            case model of
                GameOver _ _ ->
                    Element.text (viewDeathMessage game.resources)

                Running choice _ _ ->
                    Element.column [ width (px 800), height fill, padding 20, spacing 10 ]
                        [ wrapText (viewResources game.resources)
                        , wrapText ("You're currently in " ++ Location.toText game.location)
                        , case game.card of
                            Just c ->
                                case choice of
                                    None ->
                                        Element.column [ width fill ]
                                            [ wrapText c.mainText
                                            , Element.wrappedRow [ width fill, spaceEvenly ]
                                                [ Element.Input.button [ Element.alignLeft, Element.width (Element.minimum 100 fill) ] { onPress = Just (Key (ChoiceKey Left)), label = wrapText c.decisionText1 }
                                                , Element.Input.button [ Element.alignRight ] { onPress = Just (Key (ChoiceKey Right)), label = wrapText c.decisionText2 }
                                                ]
                                            ]

                                    Left ->
                                        Element.column [ width fill ]
                                            [ wrapText c.mainText
                                            , wrapText c.followUpText1
                                            , Element.Input.button [] { onPress = Just GenerateNewCard, label = Element.text "Move on" }
                                            ]

                                    Right ->
                                        Element.column [ width fill ]
                                            [ wrapText c.mainText
                                            , wrapText c.followUpText2
                                            , Element.Input.button [] { onPress = Just GenerateNewCard, label = Element.text "Move on" }
                                            ]

                            Nothing ->
                                Element.none
                        , Element.Input.button [] { onPress = Just GenerateNewCard, label = Element.text "Temporary: New Card" }
                        ]


viewBackground : Location -> Element Msg -> Html Msg
viewBackground location content =
    layout [] <|
        el
            [ Background.tiled <| Location.toImageUrl location
            , width fill
            , height fill
            , clip
            , padding 20
            ]
        <|
            content


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
        GameOver _ _ ->
            ( model, Cmd.none )

        Running choice game highscore ->
            case msg of
                NewCard newCardIndex ->
                    Debug.log (String.fromInt newCardIndex) ( Running None { game | card = Card.getCardByIndex game.currentCards newCardIndex } highscore, Cmd.none )

                Key key ->
                    ( processKey key model, Cmd.none )

                GenerateNewCard ->
                    ( model, generateCard <| List.length game.currentCards )


processKey : Key -> Model -> Model
processKey key model =
    case key of
        ChoiceKey choice ->
            case model of
                GameOver _ _ ->
                    model

                Running _ game highscore ->
                    let
                        newUnlockedCardIndexes =
                            calculateUnlockedCardIndexes game.unlockedCardIndexes choice game.card
                    in
                    Running choice
                        { game
                            | resources = calculateResourcesOnChoice game.resources choice game.location game.card
                            , unlockedCardIndexes = newUnlockedCardIndexes
                            , currentCards = getCurrentlyPossibleCards game.allCards newUnlockedCardIndexes game.location
                        }
                        (highscore + 100)

        R ->
            model

        UnknownKey ->
            model


calculateResourcesOnChoice : Resources -> Choice -> Location -> Maybe Card -> Resources
calculateResourcesOnChoice resources choice location maybeCard =
    case maybeCard of
        Nothing ->
            resources

        Just card ->
            let
                calcResources =
                    case choice of
                        Left ->
                            card.resourceChange1

                        Right ->
                            card.resourceChange2

                        None ->
                            emptyResources
            in
            Resources.combine resources calcResources
                |> Resources.combine (Location.toResourceDrain location)
                |> Resources.capResources


calculateUnlockedCardIndexes : List Int -> Choice -> Maybe Card -> List Int
calculateUnlockedCardIndexes uci choice maybeCard =
    case maybeCard of
        Nothing ->
            uci

        Just card ->
            let
                newIndexes =
                    case choice of
                        Left ->
                            card.newCards1

                        Right ->
                            card.newCards2

                        None ->
                            []

                removeIndexes =
                    case choice of
                        Left ->
                            card.removeCards1

                        Right ->
                            card.removeCards2

                        None ->
                            []
            in
            List.filter (\a -> not (List.member a removeIndexes)) uci ++ newIndexes



---- Generator ----
{- Generate an index of a card -}


generateCard : Int -> Cmd Msg
generateCard length =
    let
        generator =
            Random.int 0 (length - 1)
    in
    Random.generate NewCard generator



---- Normal functions ----
{- Creates a new List of Cards containing every card that is unlocked and from the current location -}


getCurrentlyPossibleCards : List Card -> List Int -> Location -> List Card
getCurrentlyPossibleCards allCards unlockedCardsIndexes currentLocation =
    case allCards of
        x :: xs ->
            let
                remainingList =
                    getCurrentlyPossibleCards xs unlockedCardsIndexes currentLocation
            in
            if List.member x.id unlockedCardsIndexes && List.member currentLocation x.possibleLocation then
                x :: remainingList

            else
                remainingList

        [] ->
            []



---- Helper Function ----


wrapText : String -> Element Msg
wrapText text =
    Element.paragraph [] [ Element.text text ]



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
            let
                currentCards =
                    getCurrentlyPossibleCards value.allCards value.startingCardIndexes startingLocation
            in
            ( Running Left
                { resources = startingResources
                , allCards = value.allCards
                , unlockedCardIndexes = value.startingCardIndexes
                , currentCards = currentCards
                , location = startingLocation
                , card = Nothing
                }
                0
            , generateCard <| List.length currentCards
            )

        _ ->
            ( Running Left { resources = startingResources, allCards = [], unlockedCardIndexes = [], currentCards = [], location = startingLocation, card = Nothing } 0, Cmd.none )


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
