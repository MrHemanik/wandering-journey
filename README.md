# Wandering Journey
### Small project made in Elm for the final grade in the module 'Deklarative Software Technologien' at [Hochschule Flensburg](https://hs-flensburg.de)

![Game General View](https://i.imgur.com/RL3kFCs.png)

### How to play
A playable version can be accessed at https://mrhemanik.github.io/wandering-journey/

### Brief Gameplay Description
Concept is inspired by the mobile game  "Lapse: A Forgotten Future", though it plays completely differently than Lapse.

You have 5 resources to keep up: hunger, thirst, physical health, mental health, and money. if one of them (except money) reaches 0 you have lost.
You are presented with a text prompt and the ability to choose either the left or right side. Choosing an option can add/remove resources, and items, plus it can unlock achievements.
Text prompts and their choices are saved in “cards”, which can have alternating decisions based on your items. The cards that generate can also change, either via an event line (unlocking card from choice on the previous card) or by changing the location you are currently in.

The game has a defined ending, though it can be played endless to reach a high score if one wants to.

### Setting / World Building
Wandering Journey plays in a medieval low-fantasy setting, so while magic does exist, the world is not based around it.
The world has two mayor churches/orders. The church of Mutan, standing for power and the church of fortuna, standing for luck. Being a member of the church of Mutan can lead to an unwanted symbioses with an unknown shape shifting organism for as long as you're part of the Mutan Order.
The world holds three different crystals, each colored differently

### Assets
Images in /achievements, /backgrounds and /items are self-made, while the others are from various sources
## Visuals
#### Starting Screen
Shows Resources, Location, Score, Information about the Card, Inventory, Buttons to open Controls/Achievement
![Starting Screen](https://i.imgur.com/05t5YDM.png)

#### Controls Window
![Controls](https://i.imgur.com/qUzCahr.png)
#### Achievements Window
![Achievements](https://i.imgur.com/wvnTxpL.png)
#### Delete Confirmation Window
![Delete Confirmation Window](https://i.imgur.com/HCePQ3a.png)
####Card Window
| Before Choice | After Choice |
| --- | --- |
|![Before Decision](https://i.imgur.com/ZTJ0rxz.png) | ![After Decision](https://i.imgur.com/2zZXWFS.png)|
#### Death Window
![Death Window](https://i.imgur.com/BKR5qe6.png)
## Code-Specific Information
### Custom Libraries
We use mdgriffith’s Elm-Ui for our browser UI, as it makes layouting and designing easier: https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/
### Useful links
- Elm Guide: https://guide.elm-lang.org/
- Elm Package Doc: https://package.elm-lang.org/
- Elm-Ui Doc: https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/
- Examples for Elm-Ui: https://korban.net/elm/elm-ui-patterns/
### How to compile
Run the command:
```elm make src/Main.elm --optimize --output wanderingJourney.js``` 
to compile the elm code into javaScript.
### How to run
There is a compiled version [here](https://mrhemanik.github.io/wandering-journey/). Else compile the game and run it on a (local) server. Just opening index.html doesn't work as CORS blocks access to local files, in our case our gameData like cards.

## Data Types
### Records
#### GameData
```haskell
{ items : List Item, cards : List Card, startingCardIndexes : List Int, achievements : List Achievement }
```
Every preset data that gets loaded at init and does not change. 
- Is saved per .json and gets loaded from html via [Elm Flag](https://guide.elm-lang.org/interop/flags.html)
#### Game
```haskell
{ resources : Resources, allowedCardIndexes : List Int, activeItemsIndexes : List Int, currentCards : List Card, location : Location, card : Maybe Card, nextCard : Maybe Card, score : Int}
```
Everything that is game related and can change except choice and if a run is active.

#### ViewState
```haskell
{ item : Maybe Item, showControls : Bool, showAchievements : Bool, showDeleteConfirmation : Bool, newAchievements : List Int, highlightedAchievements : List Int, selectedAchievement : Maybe Achievement, endGameText : String }
```
Saves everything variable for visualisation purposes only.
- Usage cases are for example which extra window is open, what achievements are new or what item/achievement is selected.

#### Resources
```haskell
{ hunger : Int, thirst : Int, physicalHealth : Int,mentalHealth: Int, money : Int }
```
Value from 0-100, except money which can go infinitely high. 
- Used as “health”.

#### Card
```haskell
{ id : Int, possibleLocation : List Location, mainText : String, decisionLeft : Decision, decisionRight : Decision, flags : List CardFlag }
```
Card that is fundamental for gameplay. 
- Holds information about what decisions you must take and what happens when you do. 
- Gets loaded from JSON.

#### Decision
```haskell
{ choiceText : String, pickedText : String, resourceChange : Resources, flags : List Flag }
```
Choice you can make, that has a text before and after you decide to take this option.
- Resources get added/removed and Flags are getting processed when a decision is made.
- Flags can alter variables in game, like changing what cards can be generated or what items you own

#### Item
```haskell
{ id : Int, name : String, description : String }
```
Item that a player can own inside off a run. 
- Decisions can change according to what item(s) you own. 
- Gets loaded from JSON.

#### Achievement
```haskell
{ id : Int, name : String, description : String }
```
A way to show progress. 
- Get either unlocked per flag on cards or at the end of the game via “Achievements.checkUnlock”.

#### Player
```haskell
{ unlockedAchievements : List Int, highscore : Int }
```
Progress data.
- Gets saved in and loaded from the local storage as JSON via [Elm Port](https://guide.elm-lang.org/interop/ports.html).

###Custom Types
#### Model
```haskell
= GameOver GameData Game Player ViewState
| Running GameData Game Player (Maybe Choice) ViewState
```
Holds everything currently loaded (State). 
- In retrospect should’ve been a Record with ‘screen’ custom type as an attribute to make some code parts cleaner.

#### Msg
```haskell
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
```
Event that can happen to update when the user interacts with the browser interface.
- uses the same functions as the key equivalents.

#### Location
```haskell
= Desert
| Forest
| City
| None
```
To decide from which cards you draw. 
- Every location has a “switch to x location” card.

#### Flag
```haskell
= AddItem Int
| RemoveItem Int
| AddCards (List Int)
| RemoveCards (List Int)
| ChangeLocation Location
| FollowUp Int
| UnlockAchievement Int
| TakeMoney Int
| EndGame String
| Unknown
```
Flags can be part of a decision and will be processed after a decision is made or are part of a card and will be processed when a card is generated
- AddItem/RemoveItem/AddCards/RemoveCards: Adds/Removes an item/cards from the inventory/allowedCardsIndexes
- ChangeLocation: Changes the location to the one specified
- FollowUp: Instead of generating a new card, the card with the specified id will be loaded
- UnlockAchievement: unlocks an achievement
- TakeMoney: Takes money without showing the moneyLabel and without checking if you have enough. Used for thieves that steal as much as they can
- EndGame : Ends the game with a custom message. Happens when going to next card to let them read the followUp before.

#### CardFlag
```haskell
= ConditionalDecision Condition Bool Decision
| DefaultFlag Flag
```
CardFlags are an extension of flags that can only be used on cards, not decisions
- ConditionalDecision: Replaces a decision if a condition is true. If there are multiple conditionalDecisions that can replace one decision, the last one will be taken

#### Condition
```haskell
= OwnItem Int
| OwnAllItems (List Int)
| OwnXItems Int (List Int)
| Unknown
```
Consists of conditions that a conditionalDecision can have

#### Key
```haskell
= ChoiceKey Choice
| Restart
| NumberKey Int
| Controls
| Achievements
| Delete
| Escape
| UnknownKey
```
Takes the keypad input and turns it into a Key Type for further evaluation


#### Choice
```haskell
= Left
| Right
```
Used to state which decision was taken

