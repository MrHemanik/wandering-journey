port module Main exposing (main)

import Achievement exposing (Achievement)
import Array
import Browser
import Browser.Events
import Card exposing (Card)
import CardFlag exposing (CardFlag(..))
import Choice exposing (Choice(..))
import Color exposing (color)
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



---- Ports to communicate with index.html ----


port savePlayerData : Encode.Value -> Cmd msg



---- Data Types ----


type Model
    = GameOver GameData Game Player ViewState
    | Running GameData Game Player (Maybe Choice) ViewState


type Msg
    = Key Key
    | NewCard Int
    | GenerateNewCard
    | LoadFollowUpCard
    | ToggleItemDetails Int
    | ShowControl
    | ShowAchievement
    | ShowDeleteConfirmation
    | DeletePlayerData
    | DeactivateAchievementHighlighting Int


type alias Game =
    { resources : Resources
    , allowedCardIndexes : List Int
    , activeItemsIndexes : List Int
    , currentCards : List Card
    , location : Location
    , card : Maybe Card
    , nextCard : Maybe Card
    , score : Int
    }


type alias ViewState =
    { item : Maybe Item, showControls : Bool, showAchievements : Bool, showDeleteConfirmation : Bool, newAchievements : List Int, highlightedAchievements : List Int, selectedAchievement : Maybe Achievement }


type alias GameData =
    { items : List Item, cards : List Card, startingCardIndexes : List Int, achievements : List Achievement }



---- Preset constants ----


emptyGameData =
    { items = [], cards = [], startingCardIndexes = [], achievements = [] }


emptyViewState =
    { item = Nothing, showControls = False, showAchievements = False, showDeleteConfirmation = False, newAchievements = [], highlightedAchievements = [], selectedAchievement = Nothing }


defaultGame gameData =
    { resources = startingResources
    , allowedCardIndexes = gameData.startingCardIndexes
    , activeItemsIndexes = [ 11 ]
    , currentCards = Card.getCurrentlyPossibleCards gameData.cards gameData.startingCardIndexes startingLocation
    , location = startingLocation
    , card = Nothing
    , nextCard = Nothing
    , score = 0
    }


defaultPlayer =
    { unlockedAchievements = [], highscore = 0 }


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



---- Model Helper ----


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


modelToViewState : Model -> ViewState
modelToViewState model =
    case model of
        GameOver _ _ _ state ->
            state

        Running _ _ _ _ state ->
            state



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
            modelToViewState model
    in
    viewBackground game.location <|
        column [ width fill, height fill ]
            [ column [ width fill, height fill ] <|
                if not viewState.showAchievements then
                    [ viewResources game.resources
                    , viewLocation game.location
                    , el [ paddingXY 0 5, centerX ] <| viewScore game.score "score"
                    , case ( model, viewState.showControls, viewState.showDeleteConfirmation ) of
                        ( _, True, _ ) ->
                            viewControls

                        ( _, False, True ) ->
                            viewDeleteConfirmation

                        ( Running _ _ _ _ _, _, _ ) ->
                            viewCard model

                        ( GameOver _ _ _ _, _, _ ) ->
                            viewDeathScreen model
                    ]

                else
                    [ el [ centerX, height fill, paddingXY 0 20 ] <|
                        viewAchievements (modelToGameData model) viewState (modelToPlayer model)
                    ]
            , row [ width fill ]
                [ controlsButton
                , if not viewState.showAchievements then
                    viewBag viewState game

                  else
                    el [ width fill ] <| none
                , achievementButton viewState
                ]
            ]


{-| Adds the background image to view, depending on location another background image is used
-}
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


{-| Shows the current resources
-}
viewResources : Resources -> Element Msg
viewResources resources =
    let
        resourceElement src resource isMoney =
            column
                [ width fill, paddingXY 5 20, spacing 20 ]
                [ image [ Background.color color.white, Border.rounded 3, Border.glow color.white 3, centerX, centerY ]
                    { src = src, description = "" }
                , row [ Background.color color.transBlackLight, Border.rounded 5, padding 8, width fill, defaultFont, defaultFontSize, fontColor resource isMoney ]
                    [ wrapText <| String.fromInt resource ++ extraSign isMoney ]
                ]

        fontColor resource isMoney =
            case ( resource >= 70, resource > 20, isMoney ) of
                ( True, _, False ) ->
                    Font.color color.green

                ( False, True, False ) ->
                    Font.color color.yellow

                ( False, False, False ) ->
                    Font.color color.red

                ( _, _, True ) ->
                    Font.color color.white

        extraSign isMoney =
            case isMoney of
                True ->
                    ""

                False ->
                    "%"
    in
    el [ paddingXY 0 20, width fill ] <|
        row [ Border.rounded 7, Border.width 3, Border.color color.black, Background.tiled "src/img/leather.jpg", spaceEvenly, width (minimum 400 <| maximum 800 fill), centerX ]
            [ resourceElement "src/img/resources/hunger.svg" resources.hunger False
            , resourceElement "src/img/resources/thirst.svg" resources.thirst False
            , resourceElement "src/img/resources/physicalHealth.svg" resources.physicalHealth False
            , resourceElement "src/img/resources/mentalHealth.svg" resources.mentalHealth False
            , resourceElement "src/img/resources/money.svg" resources.money True
            ]


{-| Shows the current location
<https://i.imgur.com/lZ5XZiN.png>
-}
viewLocation : Location -> Element Msg
viewLocation location =
    el [ padding 5, width (minimum 400 (maximum 800 shrink)), centerX, Background.color color.transBlack, Font.color color.white, Border.rounded 5 ] <|
        wrapText ("You are currently in a " ++ Location.toText location)


{-| Shows how far you have travelled this run
<https://i.imgur.com/WhHsrwJ.png>
-}
viewScore : Int -> String -> Element Msg
viewScore score string =
    el [ padding 5, width (minimum 400 (maximum 800 shrink)), centerX, Background.color color.transBlack, Font.color color.white, Border.rounded 5 ] <|
        if string == "score" then
            wrapText ("Meters traveled: " ++ String.fromInt score)

        else
            wrapText ("Most meters traveled: " ++ String.fromInt score)


{-| Shows the the GameOver Window
<https://i.imgur.com/W42xSBT.png>
-}
viewDeathScreen : Model -> Element Msg
viewDeathScreen model =
    let
        journeyLengthText score =
            if score <= 2500 then
                "Your short Journey ends here."

            else if score <= 7500 then
                "Your Journey ends here."

            else
                "Your long Journey ends here."
    in
    case model of
        GameOver gameData game _ _ ->
            column [ centerX, centerY, Background.color color.transWhiteHeavy, width (px 800), height (shrink |> minimum 400), padding 20, Border.rounded 7 ] <|
                [ el [ width fill, padding 20 ] <|
                    wrapText (journeyLengthText game.score)
                , wrapText (Resources.deathMessage game.resources)
                , el [ width fill, padding 20 ] <|
                    wrapText ("Distance traveled:  " ++ String.fromInt game.score ++ " meters")
                , row [ centerX, spacing 10, padding 10 ] <| achievementsInText (modelToViewState model).newAchievements gameData.achievements
                , Input.button [ width (minimum 100 fill), alignBottom ]
                    { onPress = Just (Key Restart)
                    , label = row [ centerX ] [ styledText "New ", underlineFirstCharText "Run" ]
                    }
                ]

        _ ->
            none


{-| Shows achievements from a list as an Element Msgs

achievementsInText [4][{id = 0,..},..] == <https://i.imgur.com/rsOK0LM.png>
achievementsInText [0,1][{id = 0,..},..] == <https://i.imgur.com/ux3fhhV.png>

-}
achievementsInText : List Int -> List Achievement -> List (Element Msg)
achievementsInText achievementList allAchievements =
    let
        achievementElement achievement =
            el [ Background.color color.transWhite, Border.glow color.green 2, Border.rounded 3, padding 7 ] <|
                el [ Background.color color.transBlackLight, Background.uncropped (Achievement.achievementIdToAchievementUrl achievement.id), width (px 50), height (px 50), centerX ] <|
                    image
                        [ Background.color color.transWhite, Border.glow color.transWhite 3, Border.rounded 5, width (px 20), height (px 20), alignTop ]
                        { src = "src/img/achievementStar.png"
                        , description = ""
                        }
    in
    List.map
        (\x -> row [ centerX, spacing 10 ] <| [ achievementElement x ])
        (ListHelper.idListToObjectList achievementList allAchievements)


{-| Card Window
Shows card info in every state + extra info about unlocked achievements and added/removed items from the item bag
Before choice: <https://i.imgur.com/SPCTTW9.png>
After choice: <https://i.imgur.com/4FMQxoC.png>
-}
viewCard : Model -> Element Msg
viewCard model =
    column [ centerX, centerY, Background.color color.transWhiteHeavy, width (px 800), height (shrink |> minimum 400), padding 20, Border.rounded 7 ] <|
        case model of
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
                                                    el [ Background.color color.transBlackLight, Border.rounded 3, padding 7 ] <|
                                                        el [ Background.color color.transBlackLight, Background.uncropped (Item.itemIdToImageUrl item.id), width (px 50), height (px 50), centerX ] <|
                                                            image
                                                                [ Background.color color.transWhite, Border.glow color.transWhite 3, Border.rounded 5, width (px 20), height (px 20), alignTop ]
                                                                { src =
                                                                    if bool then
                                                                        "src/img/plus.png"

                                                                    else
                                                                        "src/img/minus.png"
                                                                , description = ""
                                                                }
                                            in
                                            [ row [ width fill, paddingXY 0 20 ] [ wrapText decision.pickedText ]
                                            , row [ centerX, spacing 10, padding 10 ]
                                                (List.map itemElement (viewItemChanges decision.flags gameData.items)
                                                    ++ achievementsInText viewState.newAchievements gameData.achievements
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

            GameOver _ _ _ _ ->
                [ none ]


{-| portrays the button representing a choice
<https://i.imgur.com/xYJF6F8.png>
-}
choiceButton : Resources -> Decision -> Choice -> Element Msg
choiceButton resources decision choice =
    column [ width fill ] <|
        if Resources.isOptionAllowed resources decision.resourceChange then
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
            , paragraph [ width (minimum 100 fill), Font.center, Font.color color.red ] [ wrapText "Not enough money! " ]
            ]


{-| portrays the price of a choice that costs money
price right: <https://i.imgur.com/jCjbDxq.png>
price left: <https://i.imgur.com/NMhimI7.png>
-}
priceLabel : Int -> Element Msg
priceLabel money =
    if money < 0 then
        el [ width <| minimum 40 <| maximum 100 fill ] <|
            wrappedRow [ Background.color color.transBlackLight, Border.width 2, Border.color color.black, Border.rounded 3, centerX, padding 4 ]
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


{-| Shows the newly acquired and lost items from the bag in the card window
an example with both an item added and removed from the bag: <https://i.imgur.com/97bJW2n.png>
-}
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


{-| Control Window that shows how to control the game
<https://i.imgur.com/lBsqrom.png>
-}
viewControls : Element Msg
viewControls =
    column [ centerX, centerY, Background.color color.transWhiteHeavy, width (px 800), height (shrink |> minimum 400), padding 20, Border.rounded 7 ]
        [ row [ width fill ]
            [ el [ width (px 40) ] <| none
            , column [ centerX, width fill ]
                [ paragraph [ Font.center, defaultFont, Font.size 35 ] [ text "Controls" ] ]
            , Input.button [ Background.color color.transBlack, Font.color color.white, Border.rounded 5, padding 5 ]
                { onPress = Just ShowControl
                , label = image [ width (px 30), height (px 30), centerX ] { src = "src/img/close.svg", description = "" }
                }
            ]
        , el [ Border.width 1, Border.color color.transBlackLight, centerX, width fill ] <| none
        , wrappedRow [] [ styledText "Choose an Option: Click on the option or press", keyIcon "<-", keyIcon "->" ]
        , wrappedRow [] [ styledText "Toggle Item Details: Click on an Item or press", keyIcon "1", styledText "to", keyIcon "0" ]
        , wrappedRow [] [ styledText "Restart Game: Press", keyIcon "R" ]
        , el [ Border.width 1, Border.color color.transBlackLight, centerX, width fill ] <| none
        , wrappedRow [] [ styledText "Toggle Achievements: Click on 'Achievements' or press", keyIcon "A" ]
        , wrappedRow [] [ styledText "Toggle Controls: Click on 'Controls' or press", keyIcon "C" ]
        , wrappedRow [] [ styledText "Open Data Deletion: Click on the button in Achievements or press", keyIcon "D" ]
        , wrappedRow [] [ styledText "Close Window: Click on the X, press their button or press", keyIcon "Esc" ]
        ]


{-| Key Icon with custom string

    keyIcon "<-" == <https://i.imgur.com/zB9AkiC.png>
    keyIcon "Esc" == <https://i.imgur.com/agEe2Vi.png>

-}
keyIcon : String -> Element Msg
keyIcon keyText =
    el [ Background.uncropped "src/img/key.svg", width (px 50), height (px 50) ] <| el [ centerX, centerY, width fill, Font.center, defaultFont, Font.size 15 ] <| text keyText


{-| Achievement Window that shows what achievements there are, which are unlocked and which are newly unlocked
locked (??? cards), unlocked (normal) and highlighted (red glow): <https://i.imgur.com/r3tJwvV.png>
-}
viewAchievements : GameData -> ViewState -> Player -> Element Msg
viewAchievements gameData viewState player =
    let
        achievementElement achievement =
            el [ padding 5, width fill ] <|
                Input.button
                    ([ Background.color color.transWhiteHeavy, Border.rounded 7, width fill, centerX, padding 10 ]
                        ++ (case List.member achievement.id viewState.highlightedAchievements of
                                True ->
                                    [ Border.glow color.red 2 ]

                                False ->
                                    []
                           )
                    )
                    { onPress = Just (DeactivateAchievementHighlighting achievement.id)
                    , label =
                        row [ centerX, width fill ]
                            [ el [ Background.color color.transBlackLight, width (maximum 100 shrink), Border.rounded 3, padding 7 ] <|
                                el [ Background.uncropped (Achievement.achievementIdToAchievementUrl achievement.id), width (px 64), height (px 64), centerX ] <|
                                    if not (List.member achievement.id player.unlockedAchievements) then
                                        image
                                            [ Background.color color.transWhite, width (px 64), height (px 64), Border.rounded 5, alignTop ]
                                            { src = "src/img/lock.png", description = "" }

                                    else
                                        none
                            , column [ width fill ]
                                [ achievementText achievement achievement.name
                                , el [ Border.width 1, Border.color color.transBlackLight, centerX, width (maximum 600 fill) ] <| none
                                , achievementText achievement achievement.description
                                ]
                            ]
                    }

        achievementText achievement text =
            el [ paddingXY 10 8, centerX, centerY ] <|
                if List.member achievement.id player.unlockedAchievements then
                    wrapText text

                else
                    wrapText "???"
    in
    column [ centerX, centerY, Background.color color.transWhiteHeavy, width (px 800), height fill, padding 20, Border.rounded 7 ]
        [ row [ width fill, paddingXY 0 20 ]
            [ el [ width (px 40) ] <| none
            , paragraph [ Font.center, defaultFont, Font.size 35 ] [ text "Achievements" ]
            , Input.button [ Background.color color.transBlack, Font.color color.white, Border.rounded 5, padding 5 ]
                { onPress = Just ShowAchievement
                , label = image [ width (px 30), height (px 30), centerX ] { src = "src/img/close.svg", description = "" }
                }
            ]
        , viewScore player.highscore ""
        , el [ scrollbarY, centerX, width fill, height fill, Border.rounded 10 ] <|
            column [ width fill ] <|
                List.map achievementElement gameData.achievements
        , el [ padding 10 ] <| none
        , el [ width fill, alignBottom ] <|
            Input.button [ Background.color color.transRedHeavy, Font.color color.white, Border.rounded 5, padding 5, width fill ]
                { onPress = Just ShowDeleteConfirmation
                , label = el [ centerX ] <| underlineFirstCharText "Delete Player Data"
                }
        ]


{-| Delete Window from where you can delete your player data
<https://i.imgur.com/vfIANrn.png>
-}
viewDeleteConfirmation : Element Msg
viewDeleteConfirmation =
    column [ centerX, centerY, Background.color color.transWhiteHeavy, width (px 800), height (shrink |> minimum 400), padding 20, Border.rounded 7 ]
        [ row [ width fill ]
            [ el [ width (px 40) ] <| none
            , column [ centerX, width fill ]
                [ paragraph [ Font.center, defaultFont, Font.size 35 ] [ text "Delete" ] ]
            , Input.button [ Background.color color.transBlack, Font.color color.white, Border.rounded 5, padding 5 ]
                { onPress = Just ShowDeleteConfirmation
                , label = image [ width (px 30), height (px 30), centerX ] { src = "src/img/close.svg", description = "" }
                }
            ]
        , el [ padding 10, centerX ] <| none
        , wrapText "Are you sure you want to delete your player data? If you decide to delete your data, you will lose your highscore and unlocked achievements."
        , wrappedRow [ centerX ] [ styledText "To continue, click on the button or press", keyIcon "D" ]
        , wrappedRow [ centerX ] [ styledText "To go back, click on the X, press", keyIcon "Esc" ]
        , wrapText "or open either Achievements or Controls"
        , el [ width fill, alignBottom ] <|
            Input.button [ Background.color color.transRedHeavy, Font.color color.white, Border.rounded 5, padding 5, width fill ]
                { onPress = Just DeletePlayerData
                , label = el [ centerX ] <| underlineFirstCharText "Delete Player Data"
                }
        ]


{-| Item Bag that switches between an "all Items" and "details of item"
-}
viewBag : ViewState -> Game -> Element Msg
viewBag viewState game =
    el [ centerX, paddingXY 0 5 ] <|
        row
            ([ Background.tiled "src/img/leather.jpg", Border.rounded 7, Border.width 3, Border.color color.black, width (minimum 100 (maximum 800 fill)) ]
                ++ (case viewState.item of
                        Nothing ->
                            [ scrollbarX ]

                        Just _ ->
                            []
                   )
            )
            [ case viewState.item of
                Nothing ->
                    viewItems game.activeItemsIndexes

                Just i ->
                    viewItemDetail i
            ]


{-| Shows all items owned in the current game
2 items: <https://i.imgur.com/fcMgMB1.png>
16 items: <https://i.imgur.com/tOfruo1.png>
-}
viewItems : List Int -> Element Msg
viewItems items =
    let
        portrayAllItems itemList =
            List.map itemElement itemList

        itemElement item =
            Input.button [ width (minimum 100 fill), centerX ]
                { onPress = Just (ToggleItemDetails item)
                , label =
                    wrappedRow [ centerX ]
                        [ image
                            [ Background.color color.transBlackLight, Border.rounded 3, centerX ]
                            { src = Item.itemIdToImageUrl item
                            , description = ""
                            }
                        ]
                }
    in
    row [ centerX, height (minimum 100 shrink), width (minimum 100 shrink) ] <|
        portrayAllItems items


{-| Shows a selected item in detail
<https://i.imgur.com/V1zxy2D.png>
-}
viewItemDetail : Item -> Element Msg
viewItemDetail item =
    row [ centerX, height (minimum 100 shrink), width (minimum 400 (maximum 800 fill)), spacing 20 ]
        [ Input.button [ width fill, centerX ]
            { onPress = Just (ToggleItemDetails item.id)
            , label =
                wrappedRow [ centerX, spacing 20, padding 5, width fill ]
                    [ el [ width (maximum 100 shrink) ] <|
                        image [ Background.color color.transBlackLight, Border.rounded 3, centerX ]
                            { src = Item.itemIdToImageUrl item.id, description = "" }
                    , el [ Background.color color.transWhite, Border.rounded 7, padding 5, height (minimum 50 shrink) ] <| el [ Font.center, defaultFont, defaultFontSize, centerX, centerY ] <| text item.name
                    , el [ Background.color color.transWhite, Border.rounded 7, padding 5, width fill, height (minimum 50 shrink) ] <| el [ centerX, centerY ] <| wrapText item.description
                    ]
            }
        ]


{-| Button that opens the control window
<https://i.imgur.com/4qbPtwG.png>
-}
controlsButton : Element Msg
controlsButton =
    el [ padding 5, alignBottom, width (px 170) ] <|
        Input.button [ Background.color color.transBlack, Font.color color.white, Border.rounded 5, padding 5 ]
            { onPress = Just ShowControl
            , label = column [] [ image [ width (px 50), height (px 50), centerX ] { src = "src/img/controls.svg", description = "" }, underlineFirstCharText "Controls" ]
            }


{-| Button that opens the achievement window
normal: <https://i.imgur.com/D22iMCI.png>
on new achievement: <https://i.imgur.com/SGJkog8.png>
-}
achievementButton : ViewState -> Element Msg
achievementButton viewState =
    el [ padding 5, alignBottom, width (px 170) ] <|
        Input.button
            ([ Background.color color.transBlack, Font.color color.white, Border.rounded 5, padding 5, alignRight ]
                ++ (case length viewState.highlightedAchievements > 0 of
                        True ->
                            [ Border.glow color.red 2 ]

                        False ->
                            []
                   )
            )
            { onPress = Just ShowAchievement
            , label = column [] [ image [ width (px 50), height (px 50), centerX ] { src = "src/img/achievements.svg", description = "" }, underlineFirstCharText "Achievements" ]
            }


{-| Stylized, centered text that also wraps when there is not enough space
<https://i.imgur.com/4zdXpgl.png>
-}
wrapText : String -> Element Msg
wrapText text =
    paragraph [ Font.center, defaultFont, defaultFontSize ] [ Element.text text ]


{-| Stylized text for when you want to incorporate other elements in text
rows with styledText: <https://i.imgur.com/J78y1Fl.png>
-}
styledText : String -> Element Msg
styledText text =
    el [ Font.center, defaultFont, defaultFontSize ] <| Element.text text


{-| Returns a stylized text where the first character is underlined
-}
underlineFirstCharText : String -> Element Msg
underlineFirstCharText text =
    let
        ( firstChar, rest ) =
            ( String.left 1 text, String.dropLeft 1 text )
    in
    row []
        [ el [ Font.center, defaultFont, defaultFontSize, Font.underline ] <| Element.text firstChar
        , styledText rest
        ]



---- Update ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        isGameOver resources =
            resources.hunger <= 0 || resources.thirst <= 0 || resources.physicalHealth <= 0 || resources.mentalHealth <= 0
    in
    case ( model, isGameOver (modelToGame model).resources ) of
        ( Running gameData game player _ viewState, True ) ->
            let
                newlyUnlockedAchievements =
                    ListHelper.removeEntriesFromList (Achievement.checkUnlock game.score) player.unlockedAchievements

                updatedPlayer =
                    { player | highscore = max player.highscore game.score, unlockedAchievements = ListHelper.addEntriesToListAndSort player.unlockedAchievements newlyUnlockedAchievements }
            in
            ( GameOver gameData game updatedPlayer { viewState | newAchievements = newlyUnlockedAchievements, highlightedAchievements = ListHelper.addEntriesToListAndSort viewState.highlightedAchievements newlyUnlockedAchievements }, savePlayerData <| Player.encoder updatedPlayer )

        ( _, _ ) ->
            case ( model, msg ) of
                ( Running _ game _ _ _, NewCard newCardIndex ) ->
                    loadCard model <| Card.getCardByIndex game.currentCards newCardIndex

                ( Running _ game _ _ _, LoadFollowUpCard ) ->
                    loadCard model game.nextCard

                ( _, Key key ) ->
                    processKey key model

                ( _, GenerateNewCard ) ->
                    generatePossibleCard model

                ( Running gameData game player choice viewState, ToggleItemDetails id ) ->
                    ( Running gameData game player choice (toggleItemDetails id viewState gameData), Cmd.none )

                ( GameOver gameData game player viewState, ToggleItemDetails id ) ->
                    ( GameOver gameData game player (toggleItemDetails id viewState gameData), Cmd.none )

                ( Running gameData game player choice viewState, ShowControl ) ->
                    ( Running gameData game player choice (showControls viewState), Cmd.none )

                ( GameOver gameData game player viewState, ShowControl ) ->
                    ( GameOver gameData game player (showControls viewState), Cmd.none )

                ( Running gameData game player choice viewState, ShowAchievement ) ->
                    ( Running gameData game player choice (showAchievement viewState), Cmd.none )

                ( GameOver gameData game player viewState, ShowAchievement ) ->
                    ( GameOver gameData game player (showAchievement viewState), Cmd.none )

                ( Running gameData game player choice viewState, ShowDeleteConfirmation ) ->
                    ( Running gameData game player choice (showDeletionConfirmation viewState), Cmd.none )

                ( GameOver gameData game player viewState, ShowDeleteConfirmation ) ->
                    ( GameOver gameData game player (showDeletionConfirmation viewState), Cmd.none )

                ( _, DeletePlayerData ) ->
                    deletePlayerData model

                ( Running gameData game player choice viewState, DeactivateAchievementHighlighting id ) ->
                    ( Running gameData game player choice (deactivateAchievementHighlighting id viewState), Cmd.none )

                ( GameOver gameData game player viewState, DeactivateAchievementHighlighting id ) ->
                    ( GameOver gameData game player (deactivateAchievementHighlighting id viewState), Cmd.none )

                ( _, _ ) ->
                    ( model, Cmd.none )



---- Update extra functions: Msg functions ----


{-| Loads specified card and processes their card flags
-}
loadCard : Model -> Maybe Card -> ( Model, Cmd Msg )
loadCard model cardToLoad =
    let
        ( ( gameData, game ), ( player, viewState ) ) =
            case model of
                Running gd g p _ vs ->
                    ( ( gd, g ), ( p, vs ) )

                GameOver gd g p vs ->
                    ( ( gd, g ), ( p, vs ) )
    in
    processCardFlags (Running gameData { game | card = cardToLoad, nextCard = Nothing } player Nothing { viewState | newAchievements = [] })


{-| Generate a new card from the currentCards card pool
-}
generatePossibleCard : Model -> ( Model, Cmd Msg )
generatePossibleCard model =
    ( model, generateCard <| List.length (modelToGame model).currentCards )


{-| toggles between 'normal inventory' and 'item in detail' view of the item bag
-}
toggleItemDetails : Int -> ViewState -> GameData -> ViewState
toggleItemDetails id viewState gameData =
    { viewState
        | item =
            case viewState.item of
                Nothing ->
                    ListHelper.idToObject id gameData.items

                Just _ ->
                    Nothing
    }


{-| toggles the control window
-}
showControls : ViewState -> ViewState
showControls viewState =
    { viewState | showControls = not viewState.showControls, showAchievements = False, showDeleteConfirmation = False }


{-| toggles the achievement window
-}
showAchievement : ViewState -> ViewState
showAchievement viewState =
    { viewState
        | showAchievements = not viewState.showAchievements
        , showControls = False
        , showDeleteConfirmation = False
        , highlightedAchievements =
            if viewState.showAchievements then
                []

            else
                viewState.highlightedAchievements
    }


{-| toggles the deletion confirmation window
-}
showDeletionConfirmation : ViewState -> ViewState
showDeletionConfirmation viewState =
    { viewState | showDeleteConfirmation = not viewState.showDeleteConfirmation, showAchievements = False, showControls = False }


{-| toggles the deletion confirmation window
-}
closeWindows : ViewState -> ViewState
closeWindows viewState =
    { viewState | showDeleteConfirmation = False, showAchievements = False, showControls = False }


{-| deletes all playerData and starts a new run
-}
deletePlayerData : Model -> ( Model, Cmd Msg )
deletePlayerData model =
    let
        gameData =
            modelToGameData model

        viewState =
            modelToViewState model

        currentCards =
            Card.getCurrentlyPossibleCards gameData.cards gameData.startingCardIndexes startingLocation
    in
    if viewState.showDeleteConfirmation then
        ( Running gameData (defaultGame gameData) defaultPlayer Nothing emptyViewState
        , Cmd.batch [ savePlayerData <| Player.encoder defaultPlayer, generateCard <| List.length currentCards ]
        )

    else
        case model of
            Running gd g p c vs ->
                ( Running gd g p c (showDeletionConfirmation vs), Cmd.none )

            GameOver gd g p vs ->
                ( GameOver gd g p (showDeletionConfirmation vs), Cmd.none )


{-| removes id from achievements that are highlighted
-}
deactivateAchievementHighlighting : Int -> ViewState -> ViewState
deactivateAchievementHighlighting id viewState =
    { viewState | highlightedAchievements = ListHelper.removeEntriesFromList viewState.highlightedAchievements [ id ] }



---- Process Functions ----


{-| Takes the input key and does an action according to it
-}
processKey : Key -> Model -> ( Model, Cmd Msg )
processKey key model =
    let
        vs =
            modelToViewState model
    in
    case ( model, key, vs.showAchievements || vs.showControls || vs.showDeleteConfirmation ) of
        ( Running gameData game _ oldChoice _, ChoiceKey choice, False ) ->
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
                if Resources.isOptionAllowed game.resources resource then
                    ( Running gameData
                        { fpg
                            | resources = calculateResourcesOnChoice fpg.resources choice fpg.location fpg.card
                            , currentCards = Card.getCurrentlyPossibleCards gameData.cards fpg.allowedCardIndexes fpg.location
                            , score = fpg.score + 50
                        }
                        fpp
                        (Just choice)
                        { fpv | item = Nothing, showControls = False, showAchievements = False }
                    , flagProcessedCommand
                    )

                else
                    ( model, Cmd.none )

            else
                case game.nextCard of
                    Nothing ->
                        generatePossibleCard model

                    Just _ ->
                        loadCard model game.nextCard

        ( Running gameData _ player _ _, Restart, False ) ->
            Running gameData (defaultGame gameData) player Nothing emptyViewState |> generatePossibleCard

        ( GameOver gameData _ player _, Restart, False ) ->
            Running gameData (defaultGame gameData) player Nothing emptyViewState |> generatePossibleCard

        ( _, Delete, True ) ->
            deletePlayerData model

        ( _, NumberKey pressedNumber, _ ) ->
            let
                numberToId indexList =
                    Maybe.withDefault -1 <| Array.get (modBy 10 (pressedNumber - 1)) (Array.fromList indexList)

                newViewState gameData game viewState =
                    toggleItemDetails (numberToId game.activeItemsIndexes) viewState gameData
            in
            case model of
                GameOver gameData game player viewState ->
                    ( GameOver gameData game player (newViewState gameData game viewState), Cmd.none )

                Running gameData game player choice viewState ->
                    ( Running gameData game player choice (newViewState gameData game viewState), Cmd.none )

        ( _, Controls, _ ) ->
            case model of
                GameOver gameData game player viewState ->
                    ( GameOver gameData game player (showControls viewState), Cmd.none )

                Running gameData game player choice viewState ->
                    ( Running gameData game player choice (showControls viewState), Cmd.none )

        ( _, Achievements, _ ) ->
            case model of
                GameOver gameData game player viewState ->
                    ( GameOver gameData game player (showAchievement viewState), Cmd.none )

                Running gameData game player choice viewState ->
                    ( Running gameData game player choice (showAchievement viewState), Cmd.none )

        ( _, Escape, True ) ->
            case model of
                GameOver gameData game player viewState ->
                    ( GameOver gameData game player (closeWindows viewState), Cmd.none )

                Running gameData game player choice viewState ->
                    ( Running gameData game player choice (closeWindows viewState), Cmd.none )

        ( _, _, _ ) ->
            ( model, Cmd.none )


{-| When a choice is made:
add/remove the resources of the choice from the current resources
remove the resources the location drains
-}
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


{-| processes the flags from a card, which can be either the flags from the card itself or the flags of the decisions
-}
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
                        { game | allowedCardIndexes = ListHelper.addEntriesToListAndSort game.allowedCardIndexes list } |> (\g -> ( Running gameData g player choice viewState, cmd ))

                    RemoveCards list ->
                        { game | allowedCardIndexes = ListHelper.removeEntriesFromList game.allowedCardIndexes list } |> (\g -> ( Running gameData g player choice viewState, cmd ))

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


{-| processes the flags of the main body from a (new) card
so on a CardFlag processCardFlags processes it,
but on a general Flag it gets relayed to processFlags
-}
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
                                    case ( Condition.isCondition condition game.activeItemsIndexes, game.card, overwriteSide ) of
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


{-| checks if 'id' is from a previously not owned achievement and unlocks if that's the case
-}
checkIfIdUnlocksAchievement : Int -> Player -> ViewState -> ( Player, ViewState, Cmd Msg )
checkIfIdUnlocksAchievement id player vs =
    case List.member id player.unlockedAchievements of
        True ->
            ( player, vs, Cmd.none )

        False ->
            let
                updatedPlayer =
                    Achievement.unlockAchievement id player
            in
            ( updatedPlayer, { vs | newAchievements = [ id ], highlightedAchievements = ListHelper.addEntriesToListAndSort [ id ] vs.highlightedAchievements }, savePlayerData <| Player.encoder updatedPlayer )



---- Generator ----


{-| Generate an number between 0 and length-1
used to generate the index of a card in currentCards
-}
generateCard : Int -> Cmd Msg
generateCard length =
    let
        generator =
            Random.int 0 (length - 1)
    in
    Random.generate NewCard generator



---- Default functions ----


{-| loads data from index.html and initializes Model accordingly
-}
init : ( String, String ) -> ( Model, Cmd Msg )
init flags =
    let
        ( loadedData, playerData ) =
            flags

        gameData =
            case Data.fromResult <| Decode.decodeString gameDataDecoder loadedData of
                Data.Success value ->
                    value

                Data.Failure _ ->
                    emptyGameData

        player =
            Debug.log "player" <|
                case Data.fromResult <| Decode.decodeString Player.decoder playerData of
                    Data.Success pl ->
                        pl

                    Data.Failure _ ->
                        defaultPlayer
    in
    Running gameData (defaultGame gameData) player Nothing emptyViewState
        |> generatePossibleCard


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
