# Functional Programming Aspects of Project

The purpose of this markdown file is to show what aspects applied in this project. This part basically is highlighting some functional aspects and compare to the previous version of the code.

## Types

In general this is the core of the game. Used in defining game states and objects like ([Cards](..\src\Card.hs)) and ([Players](..\src\Player.hs)).

```haskell
data Suit = Hearts | Diamonds | Clubs | Spades | Red | Black
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Joker
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

data Powerup = Normal | PeekSelf | PeekOpponent | PeekSO | Switch | PeekSwitch | PeekDouble
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data Card = Card {rank :: Rank, suit:: Suit, powerup :: Powerup} 
  deriving (Eq, Generic, ToJSON, FromJSON)

-- =========================================================== --

data Player = Player 
    { playerId :: Int
    , hand :: Hand
    , matchPoints :: Int
    } deriving (Show, Generic, ToJSON, FromJSON)
```

## State "Bucket" vs "Finite State Machine
 
Previous version handles the game state by using a simple product type record simply a container the holds data. `Table` refers to the state of the table at each player's turn. As you can see from the definition, we cant be sure what condition, which phase, who is in turn, etc. for any state of the game. Logic of program must 'remember' it manually. 

```haskell
data Table = Table
    { players :: [Player]
    , drawDeck :: Deck
    , discardPile :: Deck
    , standings :: [(Int, Int)] 
    }
```

After refactoring, using sum type for game phases states become more explicit compared to the previous. By defining phases and action, debugging becomes more easy and adding more rules is like playing a lego, just add things that suits it.

```haskell
data GameState = GameState
    {
        players         :: [Player]
    ,   drawDeck        :: Deck
    ,   discardPile     :: Deck
    ,   currentTurn     :: Int            -- Which player moves
    ,   phase           :: GamePhase      -- Which phase took turn
    ,   logs            :: [String]
    ,   privateInfo     :: [(Int, String)]
    } deriving (Generic, ToJSON, FromJSON)
```

## IO Side Effects

The issue of making a game with functional approach is game loops is always changing the state. This violates the rule of immutability. Actions by player like changing order of cards, button input, commands and so on is a side effects. In pure functional programming as a state must be created in order to reach the immutability aspect. Previous code violates this very badly, we can justify this by looking at functions in [Actions.hs](https://github.com/mfasyas/cabo-card-functional/blob/non-drs/src/Actions.hs).

```haskell
askPlayerToAdd :: Table -> Int -> Rank -> IO Table
```

```haskell
addToPile :: Table -> Int -> IO Table
```

This is basically turning haskell to standard imperative and losing functional purity.

But, using GameAction, we can just contain the input in the Main program and just saves the output as a message to be accessible if needed ([GameEngine.hs](..\src\GameEngine.hs)) in `logs` or `privateInfo`. Now the outside world is isolated.

```haskell
updateGame :: GameState -> GameAction -> Either String GameState
updateGame ofGameState action = do
    gameRules ofGameState action      -- Rule Check | proceed if True | return error message if False   
    applyAction ofGameState action    -- Case of True, apply the action.
```

## Pure Functions

A function that gives the same output for every same given input with no side effects. To manage output like printing, save the information to either `logs` or `privateInfo` for future access so that the function itself is not giving any side effects. As an example in ([GameEngine.hs](..\src\GameEngine.hs)).

```haskell
logicInitPeek :: GameState -> Int -> Int -> Int -> Either String GameState
logicFinishTurn :: GameState -> Either String GameState
logicTimpa :: GameState -> Int -> Int -> Either String GameState
logicResolve :: GameState -> Int -> [Int] -> Either String GameState
```

## Immutability 

States cannot be modified after it is created. Instead of changing the state, create a copy with come updates. Again, mainly used in ([GameEngine.hs](..\src\GameEngine.hs)). For example, to resolve powerups take the current state and inputs of indices then return a new state by copying the current with addition to phase and privateInfo changes.

```haskell
logicResolve :: GameState -> Int -> [Int] -> Either String GameState
logicResolve ofGameState t idcs =
    case phase ofGameState of
        ResolvePowerup pt -> 
            case applyPowerupLogic ofGameState pt t idcs of
                Left e -> Left e
                Right (ns, msg) -> Right $ ns { phase = PostRoundDecision, privateInfo = [(currentTurn ofGameState, msg)] }
        _ -> Left "Bukan Powerup"
```

## Pattern Matching

Checking data against a specific structure or constant to extract its contents or decide control flow. This replaces nested `if-else` statements. You can see it in ([Card.hs](..\src\Card.hs)) for defining powerups for different cards.

```haskell
powerRules :: Rank -> Suit -> Powerup
powerRules Joker Black      = PeekDouble
powerRules Joker Red        = Normal
powerRules King Clubs       = PeekSwitch
powerRules King Spades      = PeekSwitch
powerRules King Diamonds    = Normal
powerRules King Hearts      = Normal
powerRules Jack _           = Switch
powerRules Queen _          = Switch
powerRules Seven _          = PeekSelf
powerRules Eight _          = PeekSelf
powerRules Nine _           = PeekOpponent
powerRules Ten _            = PeekSO
powerRules _ _              = Normal
```

## Higher Order Functions

Functions that take other functions as arguments or return them as results. Applied in ([GameEngine.hs](..\src\GameEngine.hs)).

```haskell
-- 'map' applies a function to every element in the list
allScores = map (\p -> (playerId p, handScore (hand p))) playerList

-- 'filter' selects elements based on a predicate function
otherPlayers = filter (\player -> playerId player /= callerId) playerList
```

## Either Monad

A structure that represents a computation that might fail. It short-circuits execution if a `Left` (error) is encountered. Used to update game states, if error is encountered return error message. 

```haskell
updateGame :: GameState -> GameAction -> Either String GameState
updateGame ofGameState action = do
    gameRules ofGameState action      -- Rule Check | proceed if True | return error message if False   
    applyAction ofGameState action    -- Case of True, apply the action.
```

## Function Composition 

Combining two functions to produce new function. Used by custome operator in ([GameRules.hs](..\src\GameRules.hs))

```haskell
(.&&.) :: Rule -> Rule -> Rule
(r1 .&&. r2) state action = r1 state action >> r2 state action

-- Usage:
gameRules = isPlayerTurn .&&. isPhaseCorrect .&&. isValidIndex
```

## Recursion

Eventhough not explicitly used, recursion occurs in basically every aspect in the game as an alternative to state mutations. It is used as the main game loop in ([Main.hs](..\app\Main.hs)) as the core game. The state is never mutated (changed). Instead, `gameLoop` finishes its job and hands off the next version of the universe (newState) to a fresh instance of `gameLoop`.

```haskell
gameLoop :: GameState -> IO ()
gameLoop state = do
    -- ... logic ...
    gameLoop newState -- The function calls itself with the NEW state
```