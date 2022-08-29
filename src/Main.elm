port module Main exposing (main)

import Achievement exposing (Achievement)
import Array
import Browser
import Browser.Events
import Card exposing (Card)
import CardFlag exposing (CardFlag(..))
import Choice exposing (Choice(..))
import Color
import Condition exposing (Condition(..))
import Data
import Decision exposing (Decision)
import DecodeHelper
import Element exposing (Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, clip, column, el, fill, height, image, layout, maximum, minimum, none, padding, paddingXY, paragraph, px, row, scrollbarX, scrollbarY, shrink, spaceEvenly, spacing, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Flag exposing (Flag(..))
import Html exposing (Html)
import Item exposing (Item)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Key exposing (Key(..))
import List exposing (length)
import ListHelper
import Location exposing (Location)
import Player exposing (Player)
import Random
import Resources exposing (Resources)


port savePlayerData : Encode.Value -> Cmd msg



---- Data Types


type alias Highscore =
    Int


type Model
    = GameOver Game Player Highscore ViewState
    | Running (Maybe Choice) Game Player Highscore ViewState


type Msg
    = Key Key
    | NewCard Int
    | GenerateNewCard
    | LoadFollowUpCard
    | ToggleItemDetails Int
    | ShowControl Bool
    | ShowAchievement Bool
    | DeletePlayerData
    | DeactivateAchievementHighlighting Int


type alias Game =
    { resources : Resources
    , allCards : List Card
    , allItems : List Item
    , allAchievements : List Achievement
    , defaultCardIndexes : List Int
    , unlockedCardIndexes : List Int
    , activeItemsIndexes : List Int
    , currentCards : List Card
    , location : Location
    , card : Maybe Card
    , nextCard : Maybe Card
    }


type alias ViewState =
    { item : Maybe Item, showControls : Bool, showAchievement : Bool, newAchievements : List Int, selectedAchievement : Maybe Achievement }


type alias JsonData =
    { items : List Item, allCards : List Card, startingCardIndexes : List Int, achievements : List Achievement }



---- Preset constants ----


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



-- Helper --


modelToGame : Model -> Game
modelToGame model =
    case model of
        Running _ game _ _ _ ->
            game

        GameOver game _ _ _ ->
            game


modelToPlayer : Model -> Player
modelToPlayer model =
    case model of
        Running _ _ player _ _ ->
            player

        GameOver _ player _ _ ->
            player



---- Decoder ----


gameDataDecoder : Decoder JsonData
gameDataDecoder =
    Decode.succeed JsonData
        |> DecodeHelper.apply (Decode.field "items" (Decode.list Item.decoder))
        |> DecodeHelper.apply (Decode.field "cards" (Decode.list Card.decoder))
        |> DecodeHelper.apply (Decode.field "startingCards" (Decode.list Decode.int))
        |> DecodeHelper.apply (Decode.field "achievements" (Decode.list Achievement.decoder))



---- Visuals ----


view : Model -> Html Msg
view model =
    let
        game =
            modelToGame model

        viewState =
            case model of
                GameOver _ _ _ state ->
                    state

                Running _ _ _ _ state ->
                    state
    in
    viewBackground game.location <|
        column [ width fill, height fill ]
            [ column [ width fill, height fill ] <|
                if not viewState.showAchievement then
                    [ viewResources game.resources
                    , viewLocation game.location
                    , if viewState.showControls then
                        viewControls viewState.showControls

                      else
                        viewCard model
                    ]

                else
                    [ el [ centerX, height fill, paddingXY 0 20 ] <|
                        viewAchievements (modelToGame model) viewState
                    ]
            , row [ width fill ]
                [ controlsButton viewState.showControls
                , if not viewState.showAchievement then
                    viewBag viewState game

                  else
                    el [ width fill ] <| none
                , achievementButton viewState.showAchievement
                ]
            ]


viewBag : ViewState -> Game -> Element Msg
viewBag viewState game =
    el [ centerX, paddingXY 0 20 ] <|
        el
            ([ Background.tiled "src/img/leather.jpg", Border.rounded 7, Border.width 3, Border.color Color.black, centerX, width (minimum 100 (maximum 800 fill)) ]
                ++ (case viewState.item of
                        Nothing ->
                            [ scrollbarX ]

                        Just _ ->
                            []
                   )
            )
        <|
            column []
                -- column is important, without it the scroll bar doesn't work right (not showing every item)
                [ case viewState.item of
                    Nothing ->
                        viewItems game.activeItemsIndexes

                    Just i ->
                        viewItemDetail i
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


viewControls : Bool -> Element Msg
viewControls showControls =
    column [ centerX, centerY, Background.color Color.transWhiteHeavy, width (px 800), height (shrink |> minimum 400), padding 20, Border.rounded 7 ]
        [ row [ width fill ]
            [ el [ width (px 40) ] <| none
            , column [ centerX, width fill ]
                [ wrapText "Controls" ]
            , Input.button [ Background.color Color.transBlack, Font.color Color.white, Border.rounded 5, padding 5 ]
                { onPress = Just (ShowControl (not showControls))
                , label = image [ width (px 30), height (px 30), centerX ] { src = "src/img/close.svg", description = "" }
                }
            ]
        , column [ width fill, paddingXY 0 25 ]
            [ textControls "Choose an Option: Click on the option or press [left/right arrow key]"
            , textControls "Toggle Item Details: Click on an Item or press [Number Key]"
            , textControls "Toggle Achievements: Click on 'Achievements' or press [A]"
            , textControls "Toggle Controls: Click on 'Controls' or press [C]"
            , textControls "Restart Game: Press [R]"
            ]
        ]


viewAchievements : Game -> ViewState -> Element Msg
viewAchievements game viewState =
    let
        portrayAllAchievements achievementList =
            [ text "" ] ++ List.map achievementElement achievementList

        achievementElement achievement =
            el [ padding 5, width fill ] <|
                Input.button
                    ([ Background.color Color.transWhiteHeavy, Border.rounded 7, width fill, centerX, padding 10 ]
                        ++ (case List.member achievement.id viewState.newAchievements of
                                True ->
                                    [ Border.glow Color.red 2 ]

                                False ->
                                    []
                           )
                    )
                    { onPress = Just (DeactivateAchievementHighlighting achievement.id)
                    , label =
                        row [ centerX, width fill ]
                            [ column [ width (maximum 100 shrink) ]
                                [ image
                                    [ Background.color Color.transBlackLight, Border.rounded 3, centerX ]
                                    { src = Achievement.achievementIdToAchievementUrl achievement.id
                                    , description = ""
                                    }
                                ]
                            , column [ width fill ]
                                [ el [ padding 5, width fill ] <| column [ Font.center, defaultFont, defaultFontSize, centerX, centerY ] [ el [ paddingXY 20 3 ] <| wrapText achievement.name, el [ Border.width 1, Border.color Color.transBlackLight, centerX, width (maximum 600 fill) ] <| none ]
                                , el [ padding 5, width fill, height (minimum 50 shrink) ] <| el [ centerX, centerY ] <| wrapText achievement.description
                                ]
                            ]
                    }
    in
    column [ centerX, centerY, Background.color Color.transWhiteHeavy, width (px 800), height fill, padding 20, Border.rounded 7 ]
        [ row [ width fill, paddingXY 0 20 ]
            [ el [ width (px 40) ] <| none
            , column [ centerX, width fill ]
                [ wrapText "Achievements" ]
            , Input.button [ Background.color Color.transBlack, Font.color Color.white, Border.rounded 5, padding 5 ]
                { onPress = Just (ShowAchievement (not viewState.showAchievement))
                , label = image [ width (px 30), height (px 30), centerX ] { src = "src/img/close.svg", description = "" }
                }
            ]
        , row [ width fill, height fill ]
            [ el [ scrollbarY, centerX, width fill, height (maximum 600 fill) ] <|
                column [] <|
                    portrayAllAchievements game.allAchievements
            ]
        , row [ width fill ]
            [ Input.button [ Background.color Color.transRedHeavy, Font.color Color.white, Border.rounded 5, padding 5, width fill ]
                { onPress = Just DeletePlayerData
                , label = wrapText "Delete Player Data"
                }
            ]
        ]


viewItemChanges : List Flag -> List Item -> List ( Maybe Item, Bool )
viewItemChanges flags items =
    case flags of
        [] ->
            [ ( Nothing, False ) ]

        x :: xs ->
            case x of
                AddItem id ->
                    ( ListHelper.idToObject id items, True ) :: viewItemChanges xs items

                RemoveItem id ->
                    ( ListHelper.idToObject id items, False ) :: viewItemChanges xs items

                _ ->
                    viewItemChanges xs items


controlsButton : Bool -> Element Msg
controlsButton showControls =
    el [ padding 5, alignBottom, width (px 170) ] <|
        Input.button [ Background.color Color.transBlack, Font.color Color.white, Border.rounded 5, padding 5 ]
            { onPress = Just (ShowControl (not showControls))
            , label = column [] [ image [ width (px 50), height (px 50), centerX ] { src = "src/img/controls.svg", description = "" }, wrapText "Controls" ]
            }


achievementButton : Bool -> Element Msg
achievementButton showAchievement =
    el [ padding 5, alignBottom, width (px 170) ] <|
        Input.button [ Background.color Color.transBlack, Font.color Color.white, Border.rounded 5, padding 5, alignRight ]
            { onPress = Just (ShowAchievement (not showAchievement))
            , label = column [] [ image [ width (px 50), height (px 50), centerX ] { src = "src/img/achievements.svg", description = "" }, wrapText "Achievements" ]
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
    row [ centerX, height (minimum 100 shrink), width (minimum 400 (maximum 800 fill)), spacing 20 ]
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
            case isGameOver game.resources of
                True ->
                    let
                        newlyUnlockedAchievements =
                            ListHelper.removeEntriesFromList (Achievement.checkDistance highscore) player.unlockedAchievements

                        updatedPlayer =
                            { player | highscore = max player.highscore highscore, unlockedAchievements = ListHelper.addEntriesToList player.unlockedAchievements newlyUnlockedAchievements }
                    in
                    ( GameOver game updatedPlayer highscore { viewState | newAchievements = Debug.log "newnew" (ListHelper.addEntriesToList viewState.newAchievements newlyUnlockedAchievements) }, savePlayerData <| Player.encoder updatedPlayer )

                False ->
                    case msg of
                        NewCard newCardIndex ->
                            processCardFlags (Running Nothing { game | card = Card.getCardByIndex game.currentCards newCardIndex } player highscore viewState)

                        LoadFollowUpCard ->
                            processCardFlags (Running Nothing { game | card = game.nextCard, nextCard = Nothing } player highscore viewState)

                        Key key ->
                            processKey key (Running choice { game | activeItemsIndexes = Debug.log "items" game.activeItemsIndexes } player highscore { viewState | newAchievements = Debug.log "new achievements: " viewState.newAchievements })

                        GenerateNewCard ->
                            ( model, generateCard <| List.length game.currentCards )

                        ToggleItemDetails id ->
                            ( Running choice game player highscore <|
                                { viewState
                                    | item =
                                        case viewState.item of
                                            Nothing ->
                                                ListHelper.idToObject id game.allItems

                                            Just _ ->
                                                Nothing
                                }
                            , Cmd.none
                            )

                        ShowControl bool ->
                            ( Running choice game player highscore { viewState | showControls = bool }, Cmd.none )

                        ShowAchievement bool ->
                            ( Running choice game player highscore { viewState | showAchievement = bool }, Cmd.none )

                        DeletePlayerData ->
                            ( Running Nothing
                                { resources = startingResources
                                , allItems = game.allItems
                                , allCards = game.allCards
                                , allAchievements = game.allAchievements
                                , defaultCardIndexes = game.defaultCardIndexes
                                , unlockedCardIndexes = game.defaultCardIndexes
                                , activeItemsIndexes = []
                                , currentCards = game.currentCards
                                , location = startingLocation
                                , card = Nothing
                                , nextCard = Nothing
                                }
                                player
                                0
                                { item = Nothing, showControls = False, showAchievement = False, newAchievements = [], selectedAchievement = Nothing }
                            , Cmd.batch [ savePlayerData <| Player.encoder { startingCards = game.defaultCardIndexes, unlockedAchievements = [], highscore = 0 }, generateCard <| List.length game.currentCards ]
                            )

                        DeactivateAchievementHighlighting int ->
                            ( Running choice game player highscore { viewState | newAchievements = ListHelper.removeEntriesFromList viewState.newAchievements [ int ] }, Cmd.none )


processKey : Key -> Model -> ( Model, Cmd Msg )
processKey key model =
    let
        gameData =
            modelToGame model

        playerData =
            modelToPlayer model
    in
    case key of
        ChoiceKey choice ->
            case model of
                GameOver _ _ _ _ ->
                    ( model, Cmd.none )

                Running oldChoice game _ highscore _ ->
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

                            ( ( fpg, fpp ), ( fpv, flagProcessedCommand ) ) =
                                case processFlags flags ( model, Cmd.none ) of
                                    ( GameOver g p _ v, cmd ) ->
                                        ( ( g, p ), ( v, cmd ) )

                                    ( Running _ g p _ v, cmd ) ->
                                        ( ( g, p ), ( v, cmd ) )
                        in
                        if isOptionAllowed game.resources resource then
                            ( Running (Just choice)
                                { fpg
                                    | resources = calculateResourcesOnChoice fpg.resources choice fpg.location fpg.card
                                    , currentCards = getCurrentlyPossibleCards fpg.allCards fpg.unlockedCardIndexes fpg.location
                                }
                                fpp
                                (highscore + 50)
                                { fpv | item = Nothing, showControls = False, showAchievement = False }
                            , flagProcessedCommand
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
                , allAchievements = gameData.allAchievements
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
                { item = Nothing, showControls = False, showAchievement = False, newAchievements = [], selectedAchievement = Nothing }
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
                                    ListHelper.idToObject x allItems
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
                            | item =
                                case ( viewState.item, intToID game.activeItemsIndexes ) of
                                    ( Nothing, Just i ) ->
                                        ListHelper.idToObject i (activeItemIndexesToItemList game.activeItemsIndexes game.allItems)

                                    ( _, _ ) ->
                                        Nothing
                        }
                    , Cmd.none
                    )

        UnknownKey ->
            ( model, Cmd.none )

        Controls ->
            case model of
                GameOver _ _ _ _ ->
                    ( model, Cmd.none )

                Running choice game player highscore viewState ->
                    ( Running choice
                        game
                        player
                        highscore
                        { viewState | showControls = not viewState.showControls }
                    , Cmd.none
                    )

        Achievements ->
            case model of
                GameOver _ _ _ _ ->
                    ( model, Cmd.none )

                Running choice game player highscore viewState ->
                    ( Running choice
                        game
                        player
                        highscore
                        { viewState | showAchievement = not viewState.showAchievement }
                    , Cmd.none
                    )


isGameOver : Resources -> Bool
isGameOver resources =
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


processFlags : List Flag -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
processFlags flags ( model, cmd ) =
    case ( flags, model ) of
        ( x :: xs, Running choice game player score viewState ) ->
            let
                resources =
                    game.resources
            in
            processFlags xs <|
                case x of
                    AddItem id ->
                        { game | activeItemsIndexes = ListHelper.addEntriesToList game.activeItemsIndexes [ id ] } |> (\g -> ( Running choice g player score viewState, cmd ))

                    RemoveItem id ->
                        { game | activeItemsIndexes = ListHelper.removeEntriesFromList game.activeItemsIndexes [ id ] } |> (\g -> ( Running choice g player score viewState, cmd ))

                    AddCards list ->
                        { game | unlockedCardIndexes = ListHelper.addEntriesToList game.unlockedCardIndexes list } |> (\g -> ( Running choice g player score viewState, cmd ))

                    RemoveCards list ->
                        { game | unlockedCardIndexes = ListHelper.removeEntriesFromList game.unlockedCardIndexes list } |> (\g -> ( Running choice g player score viewState, cmd ))

                    ChangeLocation location ->
                        { game | location = location } |> (\g -> ( Running choice g player score viewState, cmd ))

                    FollowUp id ->
                        { game | nextCard = Card.getCardById game.allCards id } |> (\g -> ( Running choice g player score viewState, cmd ))

                    UnlockAchievement id ->
                        checkIfIdUnlocksAchievement id player viewState |> (\( p, vs, command ) -> ( Running choice game p score vs, command ))

                    TakeMoney sum ->
                        { game | resources = { resources | money = max 0 (resources.money - sum) } } |> (\g -> ( Running choice g player score viewState, cmd ))

                    _ ->
                        ( model, cmd )

        ( _, _ ) ->
            ( model, cmd )


processCardFlags : Model -> ( Model, Cmd Msg )
processCardFlags inputModel =
    let
        process flags ( model, cmd ) =
            case ( flags, model ) of
                ( x :: xs, Running choice game player score viewState ) ->
                    case x of
                        ConditionalDecision condition overwriteSide decision ->
                            process xs <|
                                (\g -> ( Running choice g player score viewState, cmd )) <|
                                    case ( isCondition condition game, game.card, overwriteSide ) of
                                        ( True, Just c, False ) ->
                                            { game | card = Just { c | decisionLeft = decision } }

                                        ( True, Just c, True ) ->
                                            { game | card = Just { c | decisionRight = decision } }

                                        _ ->
                                            game

                        DefaultFlag flag ->
                            process xs (processFlags [ flag ] ( model, Cmd.none ))

                ( _, _ ) ->
                    ( model, Cmd.none )
    in
    case (modelToGame inputModel).card of
        Nothing ->
            ( inputModel, Cmd.none )

        Just card ->
            process card.flags ( inputModel, Cmd.none )


isCondition : Condition -> Game -> Bool
isCondition condition game =
    case condition of
        OwnItem id ->
            List.member id game.activeItemsIndexes

        Condition.Unknown ->
            False


checkIfIdUnlocksAchievement : Int -> Player -> ViewState -> ( Player, ViewState, Cmd Msg )
checkIfIdUnlocksAchievement id player vs =
    let
        updatedPlayer =
            Achievement.unlockAchievement id player
    in
    case List.member id player.unlockedAchievements of
        True ->
            ( player, vs, Cmd.none )

        False ->
            ( updatedPlayer, { vs | newAchievements = ListHelper.addEntriesToList [ id ] vs.newAchievements }, savePlayerData <| Player.encoder updatedPlayer )



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


textControls : String -> Element Msg
textControls text =
    paragraph [ Font.alignLeft, defaultFont, defaultFontSize, paddingXY 5 10 ] [ Element.text text ]



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
                    , allAchievements = Debug.log "achievements" value.achievements
                    , defaultCardIndexes = value.startingCardIndexes
                    , unlockedCardIndexes = value.startingCardIndexes
                    , activeItemsIndexes = []
                    , currentCards = currentCards
                    , location = startingLocation
                    , card = Nothing
                    , nextCard = Nothing
                    }

                Data.Failure _ ->
                    { resources = startingResources, allCards = [], allItems = [], allAchievements = [], defaultCardIndexes = [], unlockedCardIndexes = [], activeItemsIndexes = [], currentCards = [], location = startingLocation, card = Nothing, nextCard = Nothing }

        player =
            Debug.log "player" <|
                case Data.fromResult <| Decode.decodeString Player.decoder playerData of
                    Data.Success pl ->
                        pl

                    Data.Failure _ ->
                        { startingCards = game.defaultCardIndexes, unlockedAchievements = [], highscore = 0 }
    in
    ( Running Nothing game player 0 { item = Nothing, showControls = False, showAchievement = False, newAchievements = [], selectedAchievement = Nothing }
    , generateCard <| List.length game.currentCards
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.map Key (Browser.Events.onKeyDown Key.decoder)


main : Program ( String, String ) Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , view = view
        , update = update
        }
