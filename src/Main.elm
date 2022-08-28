port module Main exposing (main)

import Array
import Browser
import Browser.Events
import Card exposing (Card)
import CardFlag exposing (CardFlag(..))
import Color
import Condition exposing (Condition(..))
import Data
import Decision exposing (Decision)
import DecodeHelper
import Element exposing (Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, clip, column, el, fill, height, image, layout, maximum, minimum, none, padding, paddingXY, paragraph, px, row, scrollbarX, shrink, spaceEvenly, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Flag exposing (Flag(..))
import Html exposing (Html)
import Item exposing (Item)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List exposing (length)
import Location exposing (Location)
import Player exposing (Player)
import Random
import Resources exposing (Resources)


type Choice
    = Left
    | Right


type Key
    = ChoiceKey Choice
    | Restart
    | UnknownKey
    | NumberKey Int


type alias Highscore =
    Int


type Model
    = GameOver Game Player Highscore ViewState
    | Running (Maybe Choice) Game Player Highscore ViewState



-- Resources you need to manage.


type Msg
    = Key Key
    | NewCard Int
    | GenerateNewCard
    | LoadFollowUpCard
    | ToggleItemDetails Int
    | ShowControl Bool
    | ShowAchievement Bool


port savePlayerData : Encode.Value -> Cmd msg


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


type alias ViewState =
    { showDetail : Bool, item : Maybe Item, showControls : Bool, showAchievement : Bool }


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

                "1" ->
                    NumberKey 1

                "2" ->
                    NumberKey 2

                "3" ->
                    NumberKey 3

                "4" ->
                    NumberKey 4

                "5" ->
                    NumberKey 5

                "6" ->
                    NumberKey 6

                "7" ->
                    NumberKey 7

                "8" ->
                    NumberKey 8

                "9" ->
                    NumberKey 9

                "0" ->
                    NumberKey 0

                _ ->
                    UnknownKey
    in
    Decode.map toKey (Decode.field "key" Decode.string)


gameDataDecoder : Decoder JsonData
gameDataDecoder =
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
                GameOver gameState _ _ _ ->
                    gameState

                Running _ gameState _ _ _ ->
                    gameState

        viewState =
            case model of
                GameOver _ _ _ state ->
                    state

                Running _ _ _ _ state ->
                    state
    in
    viewBackground game.location <|
        column [ width fill, height fill ]
            [ viewResources game.resources
            , viewLocation game.location
            , if viewState.showControls then
                viewControls

              else if viewState.showAchievement then
                viewAchievements

              else
                viewCard model
            , row [ width fill ]
                [ controlsButton viewState.showControls
                , el [ centerX, paddingXY 0 20 ] <|
                    el [ Background.tiled "src/img/leather.jpg", Border.rounded 7, Border.width 3, Border.color Color.black, scrollbarX, centerX, width (minimum 100 (maximum 800 fill)) ] <|
                        column []
                            [ case viewState.item of
                                Nothing ->
                                    viewItems game.activeItemsIndexes

                                Just i ->
                                    if viewState.showDetail then
                                        viewItemDetail i

                                    else
                                        viewItems game.activeItemsIndexes
                            ]
                , achievementButton viewState.showAchievement
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
            ]
        <|
            content


viewResources : Resources -> Element Msg
viewResources resources =
    let
        resourceElement src resource isMoney =
            column
                [ width fill, paddingXY 5 20, spacing 20 ]
                [ image [ Background.color Color.white, Border.rounded 3, Border.glow Color.white 3, centerX, centerY ]
                    { src = src, description = "" }
                , row [ Background.color Color.transBlackLight, Border.rounded 5, padding 8, width fill, defaultFont, defaultFontSize, fontColor resource isMoney ]
                    [ wrapText <| String.fromInt resource ++ extraSign isMoney ]
                ]

        fontColor resource isMoney =
            case ( resource >= 70, resource > 20, isMoney ) of
                ( True, _, False ) ->
                    Font.color Color.green

                ( False, True, False ) ->
                    Font.color Color.yellow

                ( False, False, False ) ->
                    Font.color Color.red

                ( _, _, True ) ->
                    Font.color Color.white

        extraSign isMoney =
            case isMoney of
                True ->
                    ""

                False ->
                    "%"
    in
    el [ paddingXY 0 20, width fill ] <|
        row [ Border.rounded 7, Border.width 3, Border.color Color.black, Background.tiled "src/img/leather.jpg", spaceEvenly, width (minimum 400 <| maximum 800 fill), centerX ]
            [ resourceElement "src/img/resources/hunger.svg" resources.hunger False
            , resourceElement "src/img/resources/thirst.svg" resources.thirst False
            , resourceElement "src/img/resources/physicalHealth.svg" resources.physicalHealth False
            , resourceElement "src/img/resources/mentalHealth.svg" resources.mentalHealth False
            , resourceElement "src/img/resources/money.svg" resources.money True
            ]


viewLocation : Location -> Element Msg
viewLocation location =
    el [ padding 5, width (minimum 400 (maximum 800 shrink)), centerX, Background.color Color.transBlack, Font.color Color.white, Border.rounded 5 ] <|
        wrapText ("You are currently in a " ++ Location.toText location)


viewCard : Model -> Element Msg
viewCard model =
    let
        journeyLengthText score =
            if score <= 2500 then
                "Your short Journey ends here."

            else if score <= 7500 then
                "Your Journey ends here."

            else
                "Your long Journey ends here."
    in
    column [ centerX, centerY, Background.color Color.transWhiteHeavy, width (px 800), height (shrink |> minimum 400), padding 20, Border.rounded 7 ] <|
        case model of
            GameOver game _ score _ ->
                [ el [ width fill, padding 20 ] <|
                    wrapText (journeyLengthText score)
                , wrapText (Resources.deathMessage game.resources)
                , el [ width fill, padding 20 ] <|
                    wrapText ("Distance traveled:  " ++ String.fromInt score ++ " meters")
                , Input.button [ width (minimum 100 fill), alignBottom ]
                    { onPress = Just (Key Restart)
                    , label = wrapText "New Run"
                    }
                ]

            Running maybeChoice game _ _ _ ->
                [ case game.card of
                    Just c ->
                        column [ width fill, height fill ]
                            ([ row [ width fill, padding 20 ] [ wrapText c.mainText ] ]
                                ++ (case maybeChoice of
                                        Nothing ->
                                            [ row [ width fill, alignBottom ]
                                                [ choiceButton game.resources c.decisionLeft Left
                                                , el [ Border.width 1, height fill ] <| none
                                                , choiceButton game.resources c.decisionRight Right
                                                ]
                                            ]

                                        Just choice ->
                                            let
                                                decision =
                                                    case choice of
                                                        Left ->
                                                            c.decisionLeft

                                                        Right ->
                                                            c.decisionRight

                                                itemsAddOrRemove itemList =
                                                    row [ centerX, spacing 10 ] <| List.map itemElement itemList

                                                itemElement item =
                                                    case item of
                                                        ( Nothing, _ ) ->
                                                            none

                                                        ( Just i, bool ) ->
                                                            el [ Background.color Color.transBlackLight, Border.rounded 3, padding 7 ] <|
                                                                el [ Background.color Color.transBlackLight, Background.uncropped (Item.itemIdToImageUrl i.id), width (px 50), height (px 50), centerX ] <|
                                                                    image
                                                                        [ Background.color Color.transWhite, Border.glow Color.transWhite 3, Border.rounded 5, width (px 20), height (px 20), alignTop ]
                                                                        { src =
                                                                            if bool == True then
                                                                                "src/img/plus.png"

                                                                            else
                                                                                "src/img/minus.png"
                                                                        , description = ""
                                                                        }
                                            in
                                            [ row [ width fill, padding 20 ] [ wrapText decision.pickedText ]
                                            , case viewItemChanges decision.flags game.allItems of
                                                [] ->
                                                    none

                                                ( Nothing, _ ) :: _ ->
                                                    none

                                                list ->
                                                    itemsAddOrRemove list
                                            , row [ width fill, alignBottom ]
                                                [ Input.button [ width (minimum 100 fill) ]
                                                    { onPress =
                                                        case game.nextCard of
                                                            Nothing ->
                                                                Just GenerateNewCard

                                                            Just _ ->
                                                                Just LoadFollowUpCard
                                                    , label =
                                                        wrappedRow [ centerX, centerY ]
                                                            [ arrowLeft
                                                            , case game.nextCard of
                                                                Nothing ->
                                                                    wrapText "Move on"

                                                                Just _ ->
                                                                    wrapText "Continue"
                                                            , arrowRight
                                                            ]
                                                    }
                                                ]
                                            ]
                                   )
                            )

                    Nothing ->
                        none
                ]


viewControls : Element Msg
viewControls =
    column [ centerX, centerY, Background.color Color.transWhiteHeavy, width (px 800), height (shrink |> minimum 400), padding 20, Border.rounded 7 ]
        [ wrapText "Controls" ]


viewAchievements : Element Msg
viewAchievements =
    column [ centerX, centerY, Background.color Color.transWhiteHeavy, width (px 800), height (shrink |> minimum 400), padding 20, Border.rounded 7 ]
        [ wrapText "Achievements" ]


viewItemChanges : List Flag -> List Item -> List ( Maybe Item, Bool )
viewItemChanges flags items =
    case flags of
        [] ->
            [ ( Nothing, False ) ]

        x :: xs ->
            case x of
                AddItem id ->
                    ( Item.idToItem id items, True ) :: viewItemChanges xs items

                RemoveItem id ->
                    ( Item.idToItem id items, False ) :: viewItemChanges xs items

                _ ->
                    viewItemChanges xs items


controlsButton : Bool -> Element Msg
controlsButton showControls =
    el [ padding 5, alignBottom, width (px 160) ] <|
        Input.button [ Background.color Color.transBlack, Font.color Color.white, Border.rounded 5, padding 5 ]
            { onPress = Just (ShowControl (not showControls))
            , label = column [] [ image [ width (px 50), height (px 50), centerX ] { src = "src/img/controls.svg", description = "" }, wrapText "Controls" ]
            }


achievementButton : Bool -> Element Msg
achievementButton showAchievement =
    el [ padding 5, alignBottom, width (px 160) ] <|
        Input.button [ Background.color Color.transBlack, Font.color Color.white, Border.rounded 5, padding 5 ]
            { onPress = Just (ShowAchievement (not showAchievement))
            , label = wrappedRow [] [ image [ width (px 50), height (px 50), centerX ] { src = "src/img/achievements.svg", description = "" }, wrapText "Achievements" ]
            }


choiceButton : Resources -> Decision -> Choice -> Element Msg
choiceButton resources decision choice =
    column [ width fill ] <|
        if isOptionAllowed resources decision.resourceChange then
            [ Input.button [ width (minimum 100 fill) ]
                { onPress = Just (Key (ChoiceKey choice))
                , label =
                    column [ width fill ] <|
                        [ case choice of
                            Left ->
                                wrappedRow [ alignLeft, width fill ] [ arrowLeft, wrapText decision.choiceText, priceLabel decision.resourceChange.money ]

                            Right ->
                                wrappedRow [ alignRight, width fill, paddingXY 2 0 ] [ wrapText decision.choiceText, priceLabel decision.resourceChange.money, arrowRight ]
                        ]
                }
            ]

        else
            [ wrapText decision.choiceText
            , paragraph [ width (minimum 100 fill), Font.center, Font.color Color.red ] [ wrapText "Not enough money! " ]
            ]


priceLabel : Int -> Element Msg
priceLabel money =
    if money < 0 then
        el [ width <| minimum 40 <| maximum 100 fill ] <|
            wrappedRow [ Background.color Color.transBlackLight, Border.width 2, Border.color Color.black, Border.rounded 3, centerX, padding 4 ]
                [ wrapText <| String.fromInt money
                , image [ width (px 30), height (px 30), centerX ] { src = "src/img/resources/money.svg", description = "" }
                ]

    else
        none


arrowLeft : Element Msg
arrowLeft =
    image [ width (px 40), height (px 40) ] { src = "src/img/arrowLeft.svg", description = "" }


arrowRight : Element Msg
arrowRight =
    image [ width (px 40), height (px 40) ] { src = "src/img/arrowRight.svg", description = "" }


viewItems : List Int -> Element Msg
viewItems items =
    let
        portrayAllItems itemList =
            -- text "" as first element because when clicking the first element is always highlighted, with this an empty element will be highlighted, bypassing the highlight
            [ text "" ] ++ List.map itemElement itemList

        itemElement item =
            Input.button [ width (minimum 100 fill), centerX ]
                { onPress = Just (ToggleItemDetails item)
                , label =
                    wrappedRow [ centerX ]
                        [ image
                            [ Background.color Color.transBlackLight, Border.rounded 3, centerX ]
                            { src = Item.itemIdToImageUrl item
                            , description = ""
                            }
                        ]
                }
    in
    row [ centerX, height (minimum 100 shrink), width (minimum 100 shrink) ] <|
        portrayAllItems items


viewItemDetail : Item -> Element Msg
viewItemDetail item =
    row [ Background.tiled "src/img/leather.jpg", Border.rounded 7, Border.width 3, Border.color Color.black, centerX, height (minimum 100 shrink), width (minimum 400 (maximum 800 fill)), spacing 20 ]
        [ Input.button [ width fill, centerX ]
            { onPress = Just (ToggleItemDetails item.id)
            , label =
                wrappedRow [ centerX, spacing 20, padding 5, width fill ]
                    [ el [ width (maximum 100 shrink) ] <|
                        image [ Background.color Color.transBlackLight, Border.rounded 3, centerX ]
                            { src = Item.itemIdToImageUrl item.id, description = "" }
                    , el [ Background.color Color.transWhite, Border.rounded 7, padding 5, height (minimum 50 shrink) ] <| el [ Font.center, defaultFont, defaultFontSize, centerX, centerY ] <| text item.name
                    , el [ Background.color Color.transWhite, Border.rounded 7, padding 5, width fill, height (minimum 50 shrink) ] <| el [ centerX, centerY ] <| wrapText item.description
                    ]
            }
        ]



---- Update ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        GameOver _ _ _ _ ->
            case msg of
                Key Restart ->
                    processKey Restart model

                _ ->
                    ( model, Cmd.none )

        Running choice game player highscore viewState ->
            case checkResourcesIsZero game.resources of
                True ->
                    let
                        newPlayer =
                            { player | highscore = max player.highscore highscore }
                    in
                    ( GameOver game newPlayer highscore viewState, savePlayerData <| Player.encoder newPlayer )

                False ->
                    case msg of
                        NewCard newCardIndex ->
                            ( Running Nothing (processCardFlags { game | card = Card.getCardByIndex game.currentCards newCardIndex }) player highscore viewState, Cmd.none )

                        LoadFollowUpCard ->
                            ( Running Nothing (processCardFlags { game | card = game.nextCard, nextCard = Nothing }) player highscore viewState, Cmd.none )

                        Key key ->
                            processKey key (Running choice { game | activeItemsIndexes = Debug.log "items" game.activeItemsIndexes } player highscore viewState)

                        GenerateNewCard ->
                            ( model, generateCard <| List.length game.currentCards )

                        ToggleItemDetails id ->
                            ( Running choice game player highscore { viewState | showDetail = not viewState.showDetail, item = Item.idToItem id game.allItems }, Cmd.none )

                        ShowControl bool ->
                            ( Running choice game player highscore { viewState | showControls = bool }, Cmd.none )

                        ShowAchievement bool ->
                            ( Running choice game player highscore { viewState | showAchievement = bool }, Cmd.none )


processKey : Key -> Model -> ( Model, Cmd Msg )
processKey key model =
    let
        ( gameData, playerData ) =
            case model of
                GameOver gameState playerState _ _ ->
                    ( gameState, playerState )

                Running _ gameState playerState _ _ ->
                    ( gameState, playerState )
    in
    case key of
        ChoiceKey choice ->
            case model of
                GameOver _ _ _ _ ->
                    ( model, Cmd.none )

                Running oldChoice game player highscore _ ->
                    if oldChoice == Nothing then
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
                        if isOptionAllowed game.resources resource then
                            ( Running (Just choice)
                                { fpg
                                    | resources = calculateResourcesOnChoice fpg.resources choice fpg.location fpg.card
                                    , currentCards = getCurrentlyPossibleCards fpg.allCards fpg.unlockedCardIndexes fpg.location
                                }
                                player
                                (highscore + 50)
                                { showDetail = False, item = Nothing, showControls = False, showAchievement = False }
                            , savePlayerData <| Player.encoder player
                            )

                        else
                            ( model, Cmd.none )

                    else
                        case game.nextCard of
                            Nothing ->
                                ( model, generateCard <| List.length game.currentCards )

                            Just _ ->
                                -- Unsure if this is "clean" but feels unnecessary to define the same thing twice (and outsourcing in extra function makes it confusing)
                                update LoadFollowUpCard model

        Restart ->
            ( Running Nothing
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
                playerData
                0
                { showDetail = False, item = Nothing, showControls = False, showAchievement = False }
            , generateCard <| List.length gameData.currentCards
            )

        NumberKey int ->
            let
                intToID indexList =
                    Array.get
                        (if int > 0 then
                            int - 1

                         else
                            9
                        )
                        (Array.fromList indexList)

                activeItemIndexesToItemList indexList allItems =
                    case indexList of
                        [] ->
                            []

                        x :: xs ->
                            let
                                item =
                                    Item.idToItem x allItems
                            in
                            case item of
                                Nothing ->
                                    activeItemIndexesToItemList xs allItems

                                Just i ->
                                    i :: activeItemIndexesToItemList xs allItems
            in
            case model of
                GameOver _ _ _ _ ->
                    ( model, Cmd.none )

                Running choice game player highscore viewState ->
                    ( Running choice
                        game
                        player
                        highscore
                        { viewState
                            | showDetail = not viewState.showDetail
                            , item =
                                case intToID game.activeItemsIndexes of
                                    Just i ->
                                        Item.idToItem i (activeItemIndexesToItemList game.activeItemsIndexes game.allItems)

                                    Nothing ->
                                        Nothing
                        }
                    , Cmd.none
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


isOptionAllowed : Resources -> Resources -> Bool
isOptionAllowed gameResources choiceResources =
    (gameResources.money + choiceResources.money) >= 0


wrapText : String -> Element Msg
wrapText text =
    paragraph [ Font.center, defaultFont, defaultFontSize ] [ Element.text text ]



---- Default functions ----


init : ( String, String ) -> ( Model, Cmd Msg )
init flags =
    let
        ( gameData, playerData ) =
            flags

        game =
            case Data.fromResult <| Decode.decodeString gameDataDecoder gameData of
                Data.Success value ->
                    let
                        currentCards =
                            getCurrentlyPossibleCards value.allCards value.startingCardIndexes startingLocation
                    in
                    { resources = startingResources
                    , allItems = value.items
                    , allCards = value.allCards
                    , defaultCardIndexes = value.startingCardIndexes
                    , unlockedCardIndexes = value.startingCardIndexes
                    , activeItemsIndexes = []
                    , currentCards = currentCards
                    , location = startingLocation
                    , card = Nothing
                    , nextCard = Nothing
                    }

                Data.Failure _ ->
                    { resources = startingResources, allCards = [], allItems = [], defaultCardIndexes = [], unlockedCardIndexes = [], activeItemsIndexes = [], currentCards = [], location = startingLocation, card = Nothing, nextCard = Nothing }

        player =
            Debug.log "player" <|
                case Data.fromResult <| Decode.decodeString Player.decoder playerData of
                    Data.Success pl ->
                        pl

                    Data.Failure _ ->
                        { startingCards = game.defaultCardIndexes, unlockedAchievements = [], highscore = 0 }
    in
    ( Running Nothing game player 0 { showDetail = False, item = Nothing, showControls = False, showAchievement = False }
    , generateCard <| List.length game.currentCards
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map Key (Browser.Events.onKeyDown keyDecoder)


main : Program ( String, String ) Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }
