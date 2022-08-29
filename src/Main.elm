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


type Model
    = GameOver GameData Game Player ViewState
    | Running GameData Game Player (Maybe Choice) ViewState


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
    , unlockedCardIndexes : List Int
    , activeItemsIndexes : List Int
    , currentCards : List Card
    , location : Location
    , card : Maybe Card
    , nextCard : Maybe Card
    , score : Int
    }


type alias ViewState =
    { item : Maybe Item, showControls : Bool, showAchievement : Bool, newAchievements : List Int, highlightedAchievements : List Int, selectedAchievement : Maybe Achievement }


type alias GameData =
    { items : List Item, cards : List Card, startingCardIndexes : List Int, achievements : List Achievement }



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


modelToGameData : Model -> GameData
modelToGameData model =
    case model of
        Running gameData _ _ _ _ ->
            gameData

        GameOver gameData _ _ _ ->
            gameData


modelToGame : Model -> Game
modelToGame model =
    case model of
        Running _ game _ _ _ ->
            game

        GameOver _ game _ _ ->
            game


modelToPlayer : Model -> Player
modelToPlayer model =
    case model of
        Running _ _ player _ _ ->
            player

        GameOver _ _ player _ ->
            player



---- Decoder ----


gameDataDecoder : Decoder GameData
gameDataDecoder =
    Decode.succeed GameData
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
                        viewAchievements (modelToGameData model) viewState (modelToPlayer model)
                    ]
            , row [ width fill ]
                [ controlsButton viewState.showControls
                , if not viewState.showAchievement then
                    viewBag viewState game

                  else
                    el [ width fill ] <| none
                , achievementButton viewState.showAchievement viewState
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
            GameOver _ game _ _ ->
                [ el [ width fill, padding 20 ] <|
                    wrapText (journeyLengthText game.score)
                , wrapText (Resources.deathMessage game.resources)
                , el [ width fill, padding 20 ] <|
                    wrapText ("Distance traveled:  " ++ String.fromInt game.score ++ " meters")
                , Input.button [ width (minimum 100 fill), alignBottom ]
                    { onPress = Just (Key Restart)
                    , label = wrapText "New Run"
                    }
                ]

            Running gameData game _ maybeChoice viewState ->
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

                                                itemElement ( item, bool ) =
                                                    el [ Background.color Color.transBlackLight, Border.rounded 3, padding 7 ] <|
                                                        el [ Background.color Color.transBlackLight, Background.uncropped (Item.itemIdToImageUrl item.id), width (px 50), height (px 50), centerX ] <|
                                                            image
                                                                [ Background.color Color.transWhite, Border.glow Color.transWhite 3, Border.rounded 5, width (px 20), height (px 20), alignTop ]
                                                                { src =
                                                                    if bool then
                                                                        "src/img/plus.png"

                                                                    else
                                                                        "src/img/minus.png"
                                                                , description = ""
                                                                }

                                                achievements achievementList =
                                                    List.map
                                                        (\x -> row [ centerX, spacing 10 ] <| [ achievementElement x ])
                                                        (ListHelper.idListToObjectList achievementList gameData.achievements)

                                                achievementElement achievement =
                                                    el [ Background.color Color.transWhite, Border.glow Color.green 2, Border.rounded 3, padding 7 ] <|
                                                        el [ Background.color Color.transBlackLight, Background.uncropped (Achievement.achievementIdToAchievementUrl achievement.id), width (px 50), height (px 50), centerX ] <|
                                                            image
                                                                [ Background.color Color.transWhite, Border.glow Color.transWhite 3, Border.rounded 5, width (px 20), height (px 20), alignTop ]
                                                                { src = "src/img/achievementStar.png"
                                                                , description = ""
                                                                }
                                            in
                                            [ row [ width fill, padding 20 ] [ wrapText decision.pickedText ]
                                            , row [ centerX, spacing 10, padding 10 ]
                                                (List.map itemElement (viewItemChanges decision.flags gameData.items)
                                                    ++ achievements viewState.newAchievements
                                                )
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
    let
        keyRow text1 text2 key1 key2 =
            row []
                [ textControls text1
                , column [ Background.uncropped "src/img/key.svg", width (px 50), height (px 50) ] [ el [ centerX, centerY, width fill, Font.center, defaultFont, Font.size 20 ] <| text key1 ]
                , column [] [ wrapText text2 ]
                , if key2 == "" then
                    none

                  else
                    column [ Background.uncropped "src/img/key.svg", width (px 50), height (px 50) ] [ el [ centerX, centerY, width fill, Font.center, defaultFont, Font.size 20 ] <| text key2 ]
                ]
    in
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
            [ keyRow "Choose an Option: Click on the option or press" "" "<-" "->"
            , keyRow "Toggle Item Details: Click on an Item or press" "to" "1" "0"
            , keyRow "Toggle Achievements: Click on 'Achievements' or press" "" "A" ""
            , keyRow "Toggle Controls: Click on 'Controls' or press" "" "C" ""
            , keyRow "Restart Game: Press" "" "R" ""
            ]
        ]


viewAchievements : GameData -> ViewState -> Player -> Element Msg
viewAchievements gameData viewState player =
    let
        portrayAllAchievements achievementList =
            List.map achievementElement achievementList

        achievementElement achievement =
            el [ padding 5, width fill ] <|
                Input.button
                    ([ Background.color Color.transWhiteHeavy, Border.rounded 7, width fill, centerX, padding 10 ]
                        ++ (case List.member achievement.id viewState.highlightedAchievements of
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
                                [ el [ Background.color Color.transBlackLight, Border.rounded 3, padding 7 ] <|
                                    el [ Background.uncropped (Achievement.achievementIdToAchievementUrl achievement.id), centerX ] <|
                                        image
                                            [ Background.color Color.transWhite, width (px 64), height (px 64), Border.rounded 5, alignTop ]
                                            { src =
                                                if not (List.member achievement.id player.unlockedAchievements) then
                                                    "src/img/lock.png"

                                                else
                                                    Achievement.achievementIdToAchievementUrl achievement.id
                                            , description = ""
                                            }
                                ]
                            , column [ width fill ]
                                [ el [ padding 5, width fill ] <|
                                    column [ Font.center, defaultFont, defaultFontSize, centerX, centerY, width fill ]
                                        [ el [ paddingXY 20 3, width fill ] <|
                                            if List.member achievement.id player.unlockedAchievements then
                                                wrapText achievement.name

                                            else
                                                wrapText "???"
                                        , el [ Border.width 1, Border.color Color.transBlackLight, centerX, width (maximum 600 fill) ] <| none
                                        ]
                                , el [ padding 5, width fill, height (minimum 50 shrink) ] <|
                                    el [ centerX, centerY, width fill ] <|
                                        if List.member achievement.id player.unlockedAchievements then
                                            wrapText achievement.description

                                        else
                                            wrapText "???"
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
                column [ width fill ] <|
                    portrayAllAchievements gameData.achievements
            ]
        , row [ width fill ]
            [ Input.button [ Background.color Color.transRedHeavy, Font.color Color.white, Border.rounded 5, padding 5, width fill ]
                { onPress = Just DeletePlayerData
                , label = wrapText "Delete Player Data"
                }
            ]
        ]


viewItemChanges : List Flag -> List Item -> List ( Item, Bool )
viewItemChanges flags items =
    List.filterMap
        (\flag ->
            case flag of
                AddItem id ->
                    case ListHelper.idToObject id items of
                        Nothing ->
                            Nothing

                        Just item ->
                            Just ( item, True )

                RemoveItem id ->
                    case ListHelper.idToObject id items of
                        Nothing ->
                            Nothing

                        Just item ->
                            Just ( item, False )

                _ ->
                    Nothing
        )
        flags


controlsButton : Bool -> Element Msg
controlsButton showControls =
    el [ padding 5, alignBottom, width (px 170) ] <|
        Input.button [ Background.color Color.transBlack, Font.color Color.white, Border.rounded 5, padding 5 ]
            { onPress = Just (ShowControl (not showControls))
            , label = column [] [ image [ width (px 50), height (px 50), centerX ] { src = "src/img/controls.svg", description = "" }, wrapText "Controls" ]
            }


achievementButton : Bool -> ViewState -> Element Msg
achievementButton showAchievement viewState =
    el [ padding 5, alignBottom, width (px 170) ] <|
        Input.button
            ([ Background.color Color.transBlack, Font.color Color.white, Border.rounded 5, padding 5, alignRight ]
                ++ (case length viewState.highlightedAchievements > 0 of
                        True ->
                            [ Border.glow Color.red 2 ]

                        False ->
                            []
                   )
            )
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

        Running gameData game player choice viewState ->
            case isGameOver game.resources of
                True ->
                    let
                        newlyUnlockedAchievements =
                            ListHelper.removeEntriesFromList (Achievement.checkDistance game.score) player.unlockedAchievements

                        updatedPlayer =
                            { player | highscore = max player.highscore game.score, unlockedAchievements = ListHelper.addEntriesToList player.unlockedAchievements newlyUnlockedAchievements }
                    in
                    ( GameOver gameData game updatedPlayer { viewState | newAchievements = newlyUnlockedAchievements, highlightedAchievements = Debug.log "newnew" (ListHelper.addEntriesToList viewState.highlightedAchievements newlyUnlockedAchievements) }, savePlayerData <| Player.encoder updatedPlayer )

                False ->
                    case msg of
                        NewCard newCardIndex ->
                            processCardFlags (Running gameData { game | card = Card.getCardByIndex game.currentCards newCardIndex } player Nothing { viewState | newAchievements = [] })

                        LoadFollowUpCard ->
                            processCardFlags (Running gameData { game | card = game.nextCard, nextCard = Nothing } player Nothing viewState)

                        Key key ->
                            processKey key model

                        GenerateNewCard ->
                            ( model, generateCard <| List.length game.currentCards )

                        ToggleItemDetails id ->
                            ( Running gameData game player choice <|
                                { viewState
                                    | item =
                                        case viewState.item of
                                            Nothing ->
                                                ListHelper.idToObject id gameData.items

                                            Just _ ->
                                                Nothing
                                }
                            , Cmd.none
                            )

                        ShowControl bool ->
                            ( Running gameData game player choice { viewState | showControls = bool }, Cmd.none )

                        ShowAchievement showAch ->
                            ( Running gameData
                                game
                                player
                                choice
                                { viewState
                                    | showAchievement = showAch
                                    , highlightedAchievements =
                                        if showAch then
                                            viewState.highlightedAchievements

                                        else
                                            []
                                }
                            , Cmd.none
                            )

                        DeletePlayerData ->
                            ( Running gameData
                                { resources = startingResources
                                , unlockedCardIndexes = gameData.startingCardIndexes
                                , activeItemsIndexes = []
                                , currentCards = game.currentCards
                                , location = startingLocation
                                , card = Nothing
                                , nextCard = Nothing
                                , score = 0
                                }
                                player
                                choice
                                { item = Nothing, showControls = False, showAchievement = False, newAchievements = [], highlightedAchievements = [], selectedAchievement = Nothing }
                            , Cmd.batch [ savePlayerData <| Player.encoder { startingCards = gameData.startingCardIndexes, unlockedAchievements = [], highscore = 0 }, generateCard <| List.length game.currentCards ]
                            )

                        DeactivateAchievementHighlighting int ->
                            ( Running gameData game player choice { viewState | highlightedAchievements = ListHelper.removeEntriesFromList viewState.highlightedAchievements [ int ] }, Cmd.none )


processKey : Key -> Model -> ( Model, Cmd Msg )
processKey key model =
    let
        gameData =
            modelToGameData model

        game =
            modelToGame model

        player =
            modelToPlayer model
    in
    case key of
        ChoiceKey choice ->
            case model of
                GameOver _ _ _ _ ->
                    ( model, Cmd.none )

                Running _ _ _ oldChoice _ ->
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
                                    ( GameOver _ g p v, cmd ) ->
                                        ( ( g, p ), ( v, cmd ) )

                                    ( Running _ g p _ v, cmd ) ->
                                        ( ( g, p ), ( v, cmd ) )
                        in
                        if isOptionAllowed game.resources resource then
                            ( Running gameData
                                { fpg
                                    | resources = calculateResourcesOnChoice fpg.resources choice fpg.location fpg.card
                                    , currentCards = getCurrentlyPossibleCards gameData.cards fpg.unlockedCardIndexes fpg.location
                                    , score = fpg.score + 50
                                }
                                fpp
                                (Just choice)
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
            ( Running gameData
                { resources = startingResources
                , unlockedCardIndexes = gameData.startingCardIndexes
                , activeItemsIndexes = []
                , currentCards = game.currentCards
                , location = startingLocation
                , card = Nothing
                , nextCard = Nothing
                , score = 0
                }
                player
                Nothing
                { item = Nothing, showControls = False, showAchievement = False, newAchievements = [], highlightedAchievements = [], selectedAchievement = Nothing }
            , generateCard <| List.length game.currentCards
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

                Running _ _ _ choice viewState ->
                    ( Running gameData
                        game
                        player
                        choice
                        { viewState
                            | item =
                                case ( viewState.item, intToID game.activeItemsIndexes ) of
                                    ( Nothing, Just i ) ->
                                        ListHelper.idToObject i (activeItemIndexesToItemList game.activeItemsIndexes gameData.items)

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

                Running _ _ _ choice viewState ->
                    ( Running gameData
                        game
                        player
                        choice
                        { viewState | showControls = not viewState.showControls }
                    , Cmd.none
                    )

        Achievements ->
            case model of
                GameOver _ _ _ _ ->
                    ( model, Cmd.none )

                Running _ _ _ choice viewState ->
                    ( Running gameData
                        game
                        player
                        choice
                        { viewState
                            | showAchievement = not viewState.showAchievement
                            , highlightedAchievements =
                                if viewState.showAchievement then
                                    []

                                else
                                    viewState.highlightedAchievements
                        }
                    , Cmd.none
                    )


isGameOver : Resources -> Bool
isGameOver resources =
    resources.hunger == 0 || resources.thirst == 0 || resources.physicalHealth == 0 || resources.mentalHealth == 0


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
        ( x :: xs, Running gameData game player choice viewState ) ->
            let
                resources =
                    game.resources
            in
            processFlags xs <|
                case x of
                    AddItem id ->
                        { game | activeItemsIndexes = ListHelper.addEntriesToList game.activeItemsIndexes [ id ] } |> (\g -> ( Running gameData g player choice viewState, cmd ))

                    RemoveItem id ->
                        { game | activeItemsIndexes = ListHelper.removeEntriesFromList game.activeItemsIndexes [ id ] } |> (\g -> ( Running gameData g player choice viewState, cmd ))

                    AddCards list ->
                        { game | unlockedCardIndexes = ListHelper.addEntriesToList game.unlockedCardIndexes list } |> (\g -> ( Running gameData g player choice viewState, cmd ))

                    RemoveCards list ->
                        { game | unlockedCardIndexes = ListHelper.removeEntriesFromList game.unlockedCardIndexes list } |> (\g -> ( Running gameData g player choice viewState, cmd ))

                    ChangeLocation location ->
                        { game | location = location } |> (\g -> ( Running gameData g player choice viewState, cmd ))

                    FollowUp id ->
                        { game | nextCard = Card.getCardById gameData.cards id } |> (\g -> ( Running gameData g player choice viewState, cmd ))

                    UnlockAchievement id ->
                        checkIfIdUnlocksAchievement id player viewState |> (\( p, vs, command ) -> ( Running gameData game p choice vs, command ))

                    TakeMoney sum ->
                        { game | resources = { resources | money = max 0 (resources.money - sum) } } |> (\g -> ( Running gameData g player choice viewState, cmd ))

                    _ ->
                        ( model, cmd )

        ( _, _ ) ->
            ( model, cmd )


processCardFlags : Model -> ( Model, Cmd Msg )
processCardFlags inputModel =
    let
        process flags ( model, cmd ) =
            case ( flags, model ) of
                ( x :: xs, Running gameData game player choice viewState ) ->
                    case x of
                        ConditionalDecision condition overwriteSide newDecision ->
                            process xs <|
                                (\g -> ( Running gameData g player choice viewState, cmd )) <|
                                    case ( isCondition condition game.activeItemsIndexes, game.card, overwriteSide ) of
                                        ( True, Just c, False ) ->
                                            { game | card = Just { c | decisionLeft = newDecision } }

                                        ( True, Just c, True ) ->
                                            { game | card = Just { c | decisionRight = newDecision } }

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


isCondition : Condition -> List Int -> Bool
isCondition condition activeItemsIndexes =
    case condition of
        OwnItem id ->
            List.member id activeItemsIndexes

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
            ( updatedPlayer, { vs | newAchievements = [ id ], highlightedAchievements = ListHelper.addEntriesToList [ id ] vs.highlightedAchievements }, savePlayerData <| Player.encoder updatedPlayer )



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
        ( loadedData, playerData ) =
            flags

        ( gameData, game ) =
            case Data.fromResult <| Decode.decodeString gameDataDecoder loadedData of
                Data.Success value ->
                    let
                        currentCards =
                            getCurrentlyPossibleCards value.cards value.startingCardIndexes startingLocation
                    in
                    ( value
                    , { resources = startingResources
                      , unlockedCardIndexes = value.startingCardIndexes
                      , activeItemsIndexes = []
                      , currentCards = currentCards
                      , location = startingLocation
                      , card = Nothing
                      , nextCard = Nothing
                      , score = 0
                      }
                    )

                Data.Failure _ ->
                    ( { items = [], cards = [], startingCardIndexes = [], achievements = [] }
                    , { resources = startingResources, unlockedCardIndexes = [], activeItemsIndexes = [], currentCards = [], location = startingLocation, card = Nothing, nextCard = Nothing, score = 0 }
                    )

        player =
            Debug.log "player" <|
                case Data.fromResult <| Decode.decodeString Player.decoder playerData of
                    Data.Success pl ->
                        pl

                    Data.Failure _ ->
                        { startingCards = gameData.startingCardIndexes, unlockedAchievements = [], highscore = 0 }
    in
    ( Running gameData game player Nothing { item = Nothing, showControls = False, showAchievement = False, newAchievements = [], highlightedAchievements = [], selectedAchievement = Nothing }
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
