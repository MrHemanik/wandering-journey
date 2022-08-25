module Main exposing (main)

import Browser
import Browser.Events
import Card exposing (Card)
import Data
import DecodeHelper
import Element exposing (Element, alpha, centerX, centerY, clip, column, el, fill, height, image, inFront, layout, maximum, none, padding, paragraph, px, rgb255, rgba, row, spaceEvenly, spacing, text, width)
import Element.Background as Background exposing (color)
import Element.Border
import Element.Font as Font
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
    | Restart
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
    { resources : Resources, allCards : List Card, unlockedCardIndexes : List Int, currentCards : List Card, location : Location, card : Maybe Card, defaultCardIndexes : List Int }


type alias JsonData =
    { allCards : List Card, startingCardIndexes : List Int }



---- Preset Variables ----


defaultFont =
    Font.family
        [ Font.external
            { name = "Kalam"
            , url = "https://fonts.googleapis.com/css?family=Kalam"
            }
        ]


defaultFontSize =
    Font.size 25


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
                    Restart

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
        column [ width fill, height fill ]
            [ el [ centerX, width (px 800), padding 20 ] <| viewResources game.resources
            , el [ centerX, width (px 800) ] <|
                column [ padding 5, width (px 400), height fill, centerX, Background.color (rgba 0x00 0x00 0x00 0.6), Font.color (rgb255 0xFF 0xFF 0xFF), Element.Border.rounded 5 ]
                    [ wrapText ("You are currently in a " ++ Location.toText game.location) ]
            , el [ centerX, centerY ] <|
                case model of
                    GameOver _ highscore ->
                        column [ width (px 800), height (px 300), Background.color (rgba 0xFF 0xFF 0xFF 0.8), padding 20, Element.Border.rounded 7, centerY ]
                            [ column [ width fill, padding 20 ]
                                [ wrapText (viewDeathMessage game.resources)
                                ]
                            , column [ width fill, padding 20 ]
                                [ wrapText ("Highscore:  " ++ String.fromInt highscore) ]
                            , column [ width fill, Element.alignBottom ]
                                [ Element.Input.button [ Element.width (Element.minimum 100 fill) ]
                                    { onPress = Just (Key Restart)
                                    , label = wrapText "New Run"
                                    }
                                ]
                            ]

                    Running choice _ _ ->
                        column [ Background.color (rgba 0xFF 0xFF 0xFF 0.8), width (px 800), height (px 300), padding 20, Element.Border.rounded 7 ]
                            [ case game.card of
                                Just c ->
                                    case choice of
                                        None ->
                                            column [ width fill, height fill ]
                                                [ column [ width fill, padding 20 ]
                                                    [ wrapText c.mainText
                                                    ]
                                                , row [ width fill, Element.alignBottom ]
                                                    [ column [ width fill, spaceEvenly, centerX, centerY ]
                                                        [ if isOptionAllowed game c.resourceChange1 then
                                                            Element.Input.button [ Element.width (Element.minimum 100 fill) ]
                                                                { onPress = Just (Key (ChoiceKey Left))
                                                                , label =
                                                                    Element.wrappedRow [ Element.alignLeft ]
                                                                        [ image [ centerX, centerY ]
                                                                            { src = "src/img/arrowLeft.svg"
                                                                            , description = ""
                                                                            }
                                                                        , wrapText c.decisionText1
                                                                        ]
                                                                }

                                                          else
                                                            column [ Element.alignLeft ]
                                                                [ Element.paragraph [ Element.width (Element.minimum 100 fill), defaultFont, defaultFontSize, Font.color (rgb255 0xD0 0x31 0x2D) ] [ text "Not enough money! " ]
                                                                , wrapText c.decisionText1
                                                                ]
                                                        ]
                                                    , column [ width fill, spaceEvenly, centerX, centerY ]
                                                        [ if isOptionAllowed game c.resourceChange2 then
                                                            Element.Input.button [ Element.width (Element.minimum 100 fill) ]
                                                                { onPress = Just (Key (ChoiceKey Right))
                                                                , label =
                                                                    Element.wrappedRow [ Element.alignRight ]
                                                                        [ wrapText c.decisionText2
                                                                        , image [ centerX, centerY ]
                                                                            { src = "src/img/arrowRight.svg"
                                                                            , description = ""
                                                                            }
                                                                        ]
                                                                }

                                                          else
                                                            column [ Element.alignRight ]
                                                                [ Element.paragraph [ Element.width (Element.minimum 100 fill), defaultFont, defaultFontSize, Font.color (rgb255 0xD0 0x31 0x2D) ] [ text "Not enough money! " ]
                                                                , wrapText c.decisionText2
                                                                ]
                                                        ]
                                                    ]
                                                ]

                                        Left ->
                                            column [ width fill, height fill ]
                                                [ column [ width fill, padding 20 ]
                                                    [ wrapText c.mainText
                                                    ]
                                                , column [ width fill, padding 20 ]
                                                    [ wrapText c.followUpText1
                                                    ]
                                                , column [ width fill, Element.alignBottom ]
                                                    [ Element.Input.button [ defaultFontSize, defaultFont, Font.center, width fill ] { onPress = Just GenerateNewCard, label = Element.text "Move on" }
                                                    ]
                                                ]

                                        Right ->
                                            column [ width fill, height fill ]
                                                [ column [ width fill, padding 20 ]
                                                    [ wrapText c.mainText
                                                    ]
                                                , column [ width fill, padding 20 ]
                                                    [ wrapText c.followUpText2
                                                    ]
                                                , column [ width fill, Element.alignBottom ]
                                                    [ Element.Input.button [ defaultFontSize, defaultFont, Font.center, width fill ] { onPress = Just GenerateNewCard, label = Element.text "Move on" }
                                                    ]
                                                ]

                                Nothing ->
                                    Element.none
                            ]
            ]


viewBackground : Location -> Element Msg -> Html Msg
viewBackground location content =
    layout [] <|
        el
            [ Background.tiled <| Location.toBackgroundImageUrl location
            , width fill
            , height fill
            , clip
            , padding 20
            ]
        <|
            content


viewResources : Resources -> Element Msg
viewResources resources =
    let
        columns src resource extraSign isMoney =
            column
                [ width fill
                , padding 20
                , spacing 20
                ]
                [ image [ Background.color (rgb255 0xFF 0xFF 0xFF), Element.Border.rounded 3, Element.Border.glow (rgb255 0xFF 0xFF 0xFF) 3, centerX, centerY ]
                    { src = src
                    , description = ""
                    }
                , Element.paragraph
                    [ Background.color (rgba 0x00 0x00 0x00 0.4)
                    , Element.Border.rounded 5
                    , padding 8
                    , defaultFont
                    , defaultFontSize
                    , Font.center
                    , if resource >= 70 && isMoney == False then
                        Font.color (rgb255 0x00 0xFF 0x00)

                      else if resource > 20 && resource < 70 && isMoney == False then
                        Font.color (rgb255 0xFF 0xF0 0x00)

                      else if isMoney == False then
                        Font.color (rgb255 0xFF 0x00 0x00)

                      else
                        Font.color (rgb255 0xFF 0xFF 0xFF)
                    ]
                    [ text (String.fromInt resource ++ extraSign) ]
                ]
    in
    row [ Element.Border.rounded 7, Element.Border.width 3, Element.Border.color (rgb255 0x00 0x00 0x00), Background.tiled "src/img/leder.jpg", spaceEvenly, width fill ]
        [ columns "src/img/hunger.svg" resources.hunger "%" False
        , columns "src/img/thirst.svg" resources.thirst "%" False
        , columns "src/img/physicalHealth.svg" resources.physicalHealth "%" False
        , columns "src/img/mentalHealth.svg" resources.mentalHealth "%" False
        , columns "src/img/money.svg" resources.money "" True
        ]


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

    else
        "Died of an unknown cause"



---- Update ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        GameOver _ _ ->
            case msg of
                Key Restart ->
                    processKey Restart model

                _ ->
                    ( model, Cmd.none )

        Running _ game highscore ->
            case msg of
                NewCard newCardIndex ->
                    if checkResourcesIsZero game.resources then
                        ( GameOver game highscore, Cmd.none )

                    else
                        ( Running None { game | card = Card.getCardByIndex game.currentCards newCardIndex } highscore, Cmd.none )

                Key key ->
                    processKey key model

                GenerateNewCard ->
                    ( model, generateCard <| List.length game.currentCards )


processKey : Key -> Model -> ( Model, Cmd Msg )
processKey key model =
    let
        gameData =
            case model of
                GameOver gameState _ ->
                    gameState

                Running _ gameState _ ->
                    gameState
    in
    case key of
        ChoiceKey choice ->
            case model of
                GameOver _ _ ->
                    ( model, Cmd.none )

                Running oldChoice game highscore ->
                    let
                        newUnlockedCardIndexes =
                            calculateUnlockedCardIndexes game.unlockedCardIndexes choice game.card
                    in
                    if oldChoice == None then
                        let
                            resource =
                                case ( choice, game.card ) of
                                    ( Left, Just c ) ->
                                        c.resourceChange1

                                    ( Right, Just c ) ->
                                        c.resourceChange2

                                    ( _, _ ) ->
                                        { hunger = -100, thirst = -100, physicalHealth = -100, mentalHealth = -100, money = -100 }
                        in
                        if isOptionAllowed game resource then
                            ( Running choice
                                { game
                                    | resources = calculateResourcesOnChoice game.resources choice game.location game.card
                                    , unlockedCardIndexes = newUnlockedCardIndexes
                                    , currentCards = getCurrentlyPossibleCards game.allCards newUnlockedCardIndexes game.location
                                }
                                (highscore + 1)
                            , Cmd.none
                            )

                        else
                            ( model, Cmd.none )

                    else
                        ( model, generateCard <| List.length game.currentCards )

        Restart ->
            ( Running None
                { resources = startingResources
                , allCards = gameData.allCards
                , unlockedCardIndexes = gameData.defaultCardIndexes
                , currentCards = gameData.currentCards
                , location = startingLocation
                , card = Nothing
                , defaultCardIndexes = gameData.defaultCardIndexes
                }
                0
            , generateCard <| List.length gameData.currentCards
            )

        UnknownKey ->
            ( model, Cmd.none )


checkResourcesIsZero : Resources -> Bool
checkResourcesIsZero resources =
    if resources.hunger == 0 || resources.thirst == 0 || resources.physicalHealth == 0 || resources.mentalHealth == 0 then
        True

    else
        False


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


isOptionAllowed : Game -> Resources -> Bool
isOptionAllowed game choiceResources =
    (game.resources.money + choiceResources.money) >= 0


wrapText : String -> Element Msg
wrapText text =
    Element.paragraph [ Font.center, defaultFont, defaultFontSize ] [ Element.text text ]



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
            ( Running None
                { resources = startingResources
                , allCards = value.allCards
                , unlockedCardIndexes = value.startingCardIndexes
                , currentCards = currentCards
                , location = startingLocation
                , card = Nothing
                , defaultCardIndexes = value.startingCardIndexes
                }
                0
            , generateCard <| List.length currentCards
            )

        _ ->
            ( Running None { resources = startingResources, allCards = [], unlockedCardIndexes = [], currentCards = [], location = startingLocation, card = Nothing, defaultCardIndexes = [] } 0, Cmd.none )


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
