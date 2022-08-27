module Main exposing (main)

import Browser
import Browser.Events
import Card exposing (Card)
import CardFlag exposing (CardFlag(..))
import Condition exposing (Condition(..))
import Data
import DecodeHelper
import Element exposing (Element, alignBottom, alignLeft, alignRight, centerX, centerY, clip, column, el, fill, height, image, layout, maximum, minimum, none, padding, paddingXY, paragraph, px, rgb255, rgba, row, spaceEvenly, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Flag exposing (Flag(..))
import Html exposing (Html)
import Item exposing (Item)
import Json.Decode as Decode exposing (Decoder)
import List exposing (length)
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
    | Running Choice Game Highscore ShowItemDetail



-- Resources you need to manage.


type Msg
    = Key Key
    | NewCard Int
    | GenerateNewCard
    | LoadCard
    | ToggleItemDetails Int


type alias Game =
    { resources : Resources
    , allCards : List Card
    , allItems : List Item
    , defaultCardIndexes : List Int
    , unlockedCardIndexes : List Int
    , activeItemsIndexes : List Int
    , currentCards : List Card
    , location : Location
    , card : Maybe Card
    , nextCard : Maybe Card
    }


type alias ShowItemDetail =
    { showDetail : Bool, item : Maybe Item }


type alias JsonData =
    { items : List Item, allCards : List Card, startingCardIndexes : List Int }



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
    { hunger = 70, thirst = 70, physicalHealth = 70, mentalHealth = 70, money = 70 }


emptyResources =
    { hunger = 0, thirst = 0, physicalHealth = 0, mentalHealth = 0, money = 0 }


startingLocation =
    Location.Forest



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
        |> DecodeHelper.apply (Decode.field "items" (Decode.list Item.decoder))
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

                Running _ gameState _ _ ->
                    gameState

        showItemDetail =
            case model of
                GameOver _ _ ->
                    { showDetail = False, item = Nothing }

                Running _ _ _ showDetail ->
                    showDetail
    in
    viewBackground game.location <|
        column [ width fill, height fill ]
            [ viewResources game.resources
            , el [ centerX, width (px 800) ] <|
                column [ padding 5, width (px 400), height fill, centerX, Background.color (rgba 0x00 0x00 0x00 0.6), Font.color (rgb255 0xFF 0xFF 0xFF), Border.rounded 5 ]
                    [ wrapText ("You are currently in a " ++ Location.toText game.location) ]
            , el [ centerX, centerY ] <|
                case model of
                    GameOver _ highscore ->
                        column [ width (px 800), height (px 300), Background.color (rgba 0xFF 0xFF 0xFF 0.8), padding 20, Border.rounded 7, centerY ]
                            [ column [ width fill, padding 20 ]
                                [ wrapText (viewDeathMessage game.resources)
                                ]
                            , column [ width fill, padding 20 ]
                                [ wrapText ("Highscore:  " ++ String.fromInt highscore) ]
                            , column [ width fill, alignBottom ]
                                [ Input.button [ width (minimum 100 fill) ]
                                    { onPress = Just (Key Restart)
                                    , label = wrapText "New Run"
                                    }
                                ]
                            ]

                    Running choice _ _ _ ->
                        column [ Background.color (rgba 0xFF 0xFF 0xFF 0.8), width (px 800), height (fill |> minimum 400), padding 20, Border.rounded 7 ]
                            [ case game.card of
                                Just c ->
                                    case choice of
                                        None ->
                                            column [ width fill, height fill ]
                                                [ column [ width fill, padding 20 ]
                                                    [ wrapText c.mainText
                                                    ]
                                                , row [ width fill, alignBottom ]
                                                    [ column [ width fill, spaceEvenly, centerX, centerY ]
                                                        [ if isOptionAllowed game c.decisionLeft.resourceChange then
                                                            Input.button [ width (minimum 100 fill) ]
                                                                { onPress = Just (Key (ChoiceKey Left))
                                                                , label =
                                                                    wrappedRow [ alignLeft ]
                                                                        [ image [ width (px 40), height (px 40) ]
                                                                            { src = "src/img/arrowLeft.svg"
                                                                            , description = ""
                                                                            }
                                                                        , wrapText c.decisionLeft.choiceText
                                                                        ]
                                                                }

                                                          else
                                                            column [ alignLeft ]
                                                                [ paragraph [ width (minimum 100 fill), defaultFont, defaultFontSize, Font.color (rgb255 0xD0 0x31 0x2D) ] [ text "Not enough money! " ]
                                                                , wrapText c.decisionLeft.choiceText
                                                                ]
                                                        ]
                                                    , column [ Border.width 1, height fill ] []
                                                    , column [ width fill, spaceEvenly, centerX, centerY ]
                                                        [ if isOptionAllowed game c.decisionRight.resourceChange then
                                                            Input.button [ width (minimum 100 fill) ]
                                                                { onPress = Just (Key (ChoiceKey Right))
                                                                , label =
                                                                    wrappedRow [ alignRight ]
                                                                        [ wrapText c.decisionRight.choiceText
                                                                        , image [ width (px 40), height (px 40) ]
                                                                            { src = "src/img/arrowRight.svg"
                                                                            , description = ""
                                                                            }
                                                                        ]
                                                                }

                                                          else
                                                            column [ alignRight ]
                                                                [ paragraph [ width (minimum 100 fill), defaultFont, defaultFontSize, Font.color (rgb255 0xD0 0x31 0x2D) ] [ text "Not enough money! " ]
                                                                , wrapText c.decisionRight.choiceText
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
                                                    [ wrapText c.decisionLeft.pickedText
                                                    ]
                                                , column [ width fill, alignBottom ]
                                                    [ Input.button
                                                        [ defaultFontSize, defaultFont, width fill ]
                                                        { onPress =
                                                            case game.nextCard of
                                                                Nothing ->
                                                                    Just GenerateNewCard

                                                                Just _ ->
                                                                    Just LoadCard
                                                        , label =
                                                            wrappedRow [ centerX, centerY ]
                                                                [ image [ width (px 40), height (px 40) ]
                                                                    { src = "src/img/arrowLeft.svg"
                                                                    , description = ""
                                                                    }
                                                                , case game.nextCard of
                                                                    Nothing ->
                                                                        wrapText "Move on"

                                                                    Just _ ->
                                                                        wrapText "Continue"
                                                                , image [ width (px 40), height (px 40) ]
                                                                    { src = "src/img/arrowRight.svg"
                                                                    , description = ""
                                                                    }
                                                                ]
                                                        }
                                                    ]
                                                ]

                                        Right ->
                                            column [ width fill, height fill ]
                                                [ column [ width fill, padding 20 ]
                                                    [ wrapText c.mainText
                                                    ]
                                                , column [ width fill, padding 20 ]
                                                    [ wrapText c.decisionRight.pickedText
                                                    ]
                                                , column [ width fill, alignBottom ]
                                                    [ Input.button
                                                        [ defaultFontSize, defaultFont, Font.center, width fill ]
                                                        { onPress =
                                                            case game.nextCard of
                                                                Nothing ->
                                                                    Just GenerateNewCard

                                                                Just _ ->
                                                                    Just LoadCard
                                                        , label =
                                                            wrappedRow [ centerX, centerY ]
                                                                [ image [ width (px 40), height (px 40) ]
                                                                    { src = "src/img/arrowLeft.svg"
                                                                    , description = ""
                                                                    }
                                                                , case game.nextCard of
                                                                    Nothing ->
                                                                        wrapText "Move on"

                                                                    Just _ ->
                                                                        wrapText "Continue"
                                                                , image [ width (px 40), height (px 40) ]
                                                                    { src = "src/img/arrowRight.svg"
                                                                    , description = ""
                                                                    }
                                                                ]
                                                        }
                                                    ]
                                                ]

                                Nothing ->
                                    none
                            ]
            , case showItemDetail.item of
                Nothing ->
                    el [ centerX, width (px 800), padding 20 ] <| viewItemBag game.activeItemsIndexes

                Just i ->
                    if showItemDetail.showDetail then
                        el [ centerX, width (px 800), padding 20 ] <| viewItemDetail i

                    else
                        el [ centerX, width (px 800), padding 20 ] <| viewItemBag game.activeItemsIndexes
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
        resourceElement src resource isMoney =
            column
                [ width fill, paddingXY 5 20, spacing 20 ]
                [ image [ Background.color (rgb255 0xFF 0xFF 0xFF), Border.rounded 3, Border.glow (rgb255 0xFF 0xFF 0xFF) 3, centerX, centerY ]
                    { src = src
                    , description = ""
                    }
                , row
                    [ Background.color (rgba 0x00 0x00 0x00 0.4)
                    , Border.rounded 5
                    , padding 8
                    , width fill
                    , defaultFont
                    , defaultFontSize
                    , case ( resource >= 70, resource > 20, isMoney ) of
                        ( True, _, False ) ->
                            Font.color (rgb255 0x00 0xFF 0x00)

                        ( False, True, False ) ->
                            Font.color (rgb255 0xFF 0xF0 0x00)

                        ( False, False, False ) ->
                            Font.color (rgb255 0xFF 0x00 0x00)

                        ( _, _, True ) ->
                            Font.color (rgb255 0xFF 0xFF 0xFF)
                    ]
                    [ wrapText
                        (String.fromInt resource
                            ++ (case isMoney of
                                    True ->
                                        ""

                                    False ->
                                        "%"
                               )
                        )
                    ]
                ]
    in
    el [ paddingXY 0 20, width fill ] <|
        row [ Border.rounded 7, Border.width 3, Border.color (rgb255 0x00 0x00 0x00), Background.tiled "src/img/leather.jpg", spaceEvenly, width (minimum 400 <| maximum 800 fill), centerX ]
            [ resourceElement "src/img/resources/hunger.svg" resources.hunger False
            , resourceElement "src/img/resources/thirst.svg" resources.thirst False
            , resourceElement "src/img/resources/physicalHealth.svg" resources.physicalHealth False
            , resourceElement "src/img/resources/mentalHealth.svg" resources.mentalHealth False
            , resourceElement "src/img/resources/money.svg" resources.money True
            ]


viewItemBag : List Int -> Element Msg
viewItemBag items =
    let
        portrayAllItems itemList =
            -- text "" as first element because when clicking the first element is always highlighted, with this an empty element will be highlighted, bypassing the highlight
            [ text "" ] ++ List.map itemElement itemList

        itemElement item =
            Input.button [ width (minimum 100 fill), centerX, padding 20 ]
                { onPress = Just (ToggleItemDetails item)
                , label =
                    wrappedRow [ centerX ]
                        [ image
                            [ Background.color (rgba 0x00 0x00 0x00 0.4), Border.rounded 3, centerX ]
                            { src = Item.itemIdToImageUrl item
                            , description = ""
                            }
                        ]
                }
    in
    row
        ([ Border.rounded 7, Border.width 3, Border.color (rgb255 0x00 0x00 0x00), Background.tiled "src/img/leather.jpg", spaceEvenly, centerX ]
            ++ (if List.length items > 0 then
                    [ height fill ]

                else
                    [ height (px 100), width (px 100) ]
               )
        )
    <|
        portrayAllItems items


viewItemDetail : Item -> Element Msg
viewItemDetail item =
    row [ Background.tiled "src/img/leather.jpg", Border.width 3, Border.color (rgb255 0x00 0x00 0x00), Border.rounded 3, centerX, width fill, height fill, spacing 20 ]
        [ Input.button [ width (minimum 100 fill), centerX ]
            { onPress = Just (ToggleItemDetails item.id)
            , label =
                wrappedRow [ centerX, spacing 20, width fill ]
                    [ column [ padding 20, Border.rounded 7 ]
                        [ image
                            [ Background.color (rgba 0x00 0x00 0x00 0.4), Border.rounded 3, centerX ]
                            { src = Item.itemIdToImageUrl item.id
                            , description = ""
                            }
                        ]
                    , column [ Background.color (rgba 0xFF 0xFF 0xFF 0.6), Border.rounded 7, padding 20, width (maximum 250 fill) ] [ wrapText item.name ]
                    , column [ Background.color (rgba 0xFF 0xFF 0xFF 0.6), Border.rounded 7, padding 20, width (maximum 350 fill) ] [ wrapText item.description ]
                    ]
            }
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

        Running choice game highscore show ->
            case msg of
                NewCard newCardIndex ->
                    if checkResourcesIsZero game.resources then
                        ( GameOver game highscore, Cmd.none )

                    else
                        ( Running None (processCardFlags { game | card = Card.getCardByIndex game.currentCards newCardIndex }) highscore show, Cmd.none )

                LoadCard ->
                    if checkResourcesIsZero game.resources then
                        ( GameOver game highscore, Cmd.none )

                    else
                        Debug.log "load" ( Running None (processCardFlags { game | card = game.nextCard, nextCard = Nothing }) highscore show, Cmd.none )

                Key key ->
                    processKey key (Running choice { game | activeItemsIndexes = Debug.log "items" game.activeItemsIndexes } highscore show)

                GenerateNewCard ->
                    ( model, generateCard <| List.length game.currentCards )

                ToggleItemDetails id ->
                    ( Running choice game highscore { show | showDetail = not show.showDetail, item = Item.idToItem id game.allItems }, Cmd.none )


processKey : Key -> Model -> ( Model, Cmd Msg )
processKey key model =
    let
        gameData =
            case model of
                GameOver gameState _ ->
                    gameState

                Running _ gameState _ _ ->
                    gameState
    in
    case key of
        ChoiceKey choice ->
            case model of
                GameOver _ _ ->
                    ( model, Cmd.none )

                Running oldChoice game highscore show ->
                    if oldChoice == None then
                        let
                            ( resource, flags ) =
                                case ( choice, game.card ) of
                                    ( Left, Just c ) ->
                                        ( c.decisionLeft.resourceChange, c.decisionLeft.flags )

                                    ( Right, Just c ) ->
                                        ( c.decisionRight.resourceChange, c.decisionRight.flags )

                                    ( _, _ ) ->
                                        ( { hunger = -100, thirst = -100, physicalHealth = -100, mentalHealth = -100, money = -100 }, [] )

                            fpg =
                                processFlags flags game
                        in
                        if isOptionAllowed game resource then
                            ( Running choice
                                { fpg
                                    | resources = calculateResourcesOnChoice fpg.resources choice fpg.location fpg.card
                                    , currentCards = getCurrentlyPossibleCards fpg.allCards fpg.unlockedCardIndexes fpg.location
                                }
                                (highscore + 1)
                                { showDetail = False, item = Nothing }
                            , Cmd.none
                            )

                        else
                            ( model, Cmd.none )

                    else
                        case game.nextCard of
                            Nothing ->
                                ( model, generateCard <| List.length game.currentCards )

                            Just _ ->
                                if checkResourcesIsZero game.resources then
                                    ( GameOver game highscore, Cmd.none )

                                else
                                    Debug.log "load" ( Running None (processCardFlags { game | card = game.nextCard, nextCard = Nothing }) highscore show, Cmd.none )

        Restart ->
            ( Running None
                { resources = startingResources
                , allItems = gameData.allItems
                , allCards = gameData.allCards
                , defaultCardIndexes = gameData.defaultCardIndexes
                , unlockedCardIndexes = gameData.defaultCardIndexes
                , activeItemsIndexes = []
                , currentCards = gameData.currentCards
                , location = startingLocation
                , card = Nothing
                , nextCard = Nothing
                }
                0
                { showDetail = False, item = Nothing }
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
                            card.decisionLeft.resourceChange

                        Right ->
                            card.decisionRight.resourceChange

                        None ->
                            emptyResources
            in
            Resources.combine resources calcResources
                |> Resources.combine (Location.toResourceDrain location)
                |> Resources.capResources


removeEntriesFromList : List a -> List a -> List a
removeEntriesFromList list removeList =
    List.filter (\a -> not (List.member a removeList)) list


addEntriesToList : List comparable -> List comparable -> List comparable
addEntriesToList list addList =
    case addList of
        [] ->
            list

        x :: xs ->
            if List.member x list then
                addEntriesToList list xs

            else
                addEntriesToList (List.sort (x :: list)) xs


processFlags : List Flag -> Game -> Game
processFlags flags game =
    case flags of
        [] ->
            game

        x :: xs ->
            case x of
                AddItem id ->
                    processFlags xs { game | activeItemsIndexes = addEntriesToList game.activeItemsIndexes [ id ] }

                RemoveItem id ->
                    processFlags xs { game | activeItemsIndexes = removeEntriesFromList game.activeItemsIndexes [ id ] }

                AddCards list ->
                    processFlags xs { game | unlockedCardIndexes = addEntriesToList game.unlockedCardIndexes list }

                RemoveCards list ->
                    processFlags xs { game | unlockedCardIndexes = removeEntriesFromList game.unlockedCardIndexes list }

                ChangeLocation location ->
                    processFlags xs { game | location = location }

                FollowUp id ->
                    processFlags xs { game | nextCard = Card.getCardById game.allCards id }

                _ ->
                    Debug.log "Unknown Flag detected" processFlags xs game


processCardFlags : Game -> Game
processCardFlags inputGame =
    let
        process flags game =
            case flags of
                [] ->
                    game

                x :: xs ->
                    case x of
                        ConditionalDecision condition overwriteSide decision ->
                            process xs <|
                                case ( isConditionTrue condition game, game.card, overwriteSide ) of
                                    ( True, Just c, False ) ->
                                        Debug.log "left" { game | card = Just { c | decisionLeft = decision } }

                                    ( True, Just c, True ) ->
                                        Debug.log "right" { game | card = Just { c | decisionRight = decision } }

                                    _ ->
                                        game

                        DefaultFlag flag ->
                            process xs (processFlags [ flag ] game)
    in
    case inputGame.card of
        Nothing ->
            Debug.log "No card to process cardFlags from" inputGame

        Just card ->
            process card.flags inputGame


isConditionTrue : Condition -> Game -> Bool
isConditionTrue condition game =
    case condition of
        OwnItem id ->
            List.member id game.activeItemsIndexes

        Condition.Unknown ->
            False



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
    paragraph [ Font.center, defaultFont, defaultFontSize ] [ Element.text text ]



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
                , allItems = value.items
                , allCards = value.allCards
                , defaultCardIndexes = value.startingCardIndexes
                , unlockedCardIndexes = value.startingCardIndexes
                , activeItemsIndexes = [ 0, 1, 2, 3 ]
                , currentCards = currentCards
                , location = startingLocation
                , card = Nothing
                , nextCard = Nothing
                }
                0
                { showDetail = False, item = Nothing }
            , generateCard <| List.length currentCards
            )

        _ ->
            Debug.log "Failed to load Data" ( Running None { resources = startingResources, allCards = [], allItems = [], defaultCardIndexes = [], unlockedCardIndexes = [], activeItemsIndexes = [], currentCards = [], location = startingLocation, card = Nothing, nextCard = Nothing } 0 { showDetail = False, item = Nothing }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map Key (Browser.Events.onKeyDown keyDecoder)


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }
