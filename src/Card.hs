module Card where

-- import Foreign (Storable(peek))
-- import GHC.Base (build)
import Data.List (intercalate)
import System.Random (randomRIO)
import Data.Array.IO (IOArray, newListArray, readArray, writeArray)
import Control.Monad (forM)

data Suit = Hearts | Diamonds | Clubs | Spades | Red | Black
  deriving (Show, Eq)

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Joker
  deriving (Show, Eq, Ord, Enum, Bounded)

data Powerup = Normal | PeekSelf | PeekOpponent | PeekSO | Switch | PeekSwitch | PeekDouble
  deriving (Show, Eq)

data Card = Card {rank :: Rank, suit:: Suit, powerup :: Powerup} deriving (Eq)

type Deck = [Card]

newtype Hand = Hand [Card] deriving (Eq)

instance Show Hand where
    show (Hand hs) = "[" ++ intercalate ", " (map showCardRS hs) ++ "]"

instance Show Card where
    show (Card r s _) = show r ++ " of " ++ show s

{-
  - PeekSelf        -- open 1 card from player's hand
  - PeekOpponent    -- open 1 card from opponent's hand
  - PeekSO          -- open 1 card from player and opponent hand
  - Switch          -- switch 1 card with opponent without looking the card
  - PeekSwitch      -- open 1 card from hand and opponent. Switch 1 card with opponent
  - PeekDouble      -- open 2 card from hand and opponent

  =========================================== Card Info ===========================================
  1-6 : Normal        , normal value
  7/8 : PeekSelf      , normal value
  9   : PeekOpponent  , normal value
  10  : PeekSO        , normal value
  J/Q : Switch        , value 11
  Kred: Normal        , value 0
  Kblk: PeekSwitch    , value 12
  BJ  : PeekDouble    , value 15
  RJ  : Normal        , value -1
-}

-- =============================================================================
-- Lists of functions

valueRules :: Rank -> Suit -> Int
valueRules Joker Red         = -1
valueRules Joker Black       = 15
valueRules Jack _            = 11
valueRules Queen _           = 11
valueRules King s | s `elem` [Hearts, Diamonds] = 0
valueRules King s | s `elem` [Clubs,  Spades]   = 12
valueRules r _ = fromEnum r + 1 

powerRules :: Rank -> Suit -> Powerup
powerRules Joker Black      = PeekDouble
powerRules Joker Red        = Normal
powerRules King s | s `elem` [Clubs, Spades] = PeekSwitch
powerRules King _           = Normal
powerRules Jack _           = Switch
powerRules Queen _          = Switch
powerRules Seven _          = PeekSelf
powerRules Eight _          = PeekSelf
powerRules Nine _           = PeekOpponent
powerRules Ten _            = PeekSO
powerRules _ _              = Normal

cardValue :: Card -> Int
cardValue (Card r s _) = valueRules r s

buildDeck :: Deck
buildDeck = standard ++ jokers
  where
    standard =
      [ Card r s (powerRules r s)
      | r <- [Ace .. King]
      , s <- [Hearts, Diamonds, Clubs, Spades]
      ]

    jokers =
      [ Card Joker Red  (powerRules Joker Red)
      , Card Joker Red   (powerRules Joker Red)
      , Card Joker Black (powerRules Joker Black)
      , Card Joker Black (powerRules Joker Black)
      ]

showCardRS :: Card -> String
showCardRS (Card r s _) = show r ++ " of " ++ show s -- to be deleted

drawCard :: Deck -> (Card, Deck)
drawCard [] = error "Cannot draw from an empty deck!"
drawCard (topCard : restOfDeck) = (topCard, restOfDeck)

-- A Fisher-Yates shuffle function using your IO imports
shuffleDeck :: Deck -> IO Deck
shuffleDeck cards = do
    let len = length cards
    -- Create a mutable array from the card list
    arr <- newListArray (0, len - 1) cards :: IO (IOArray Int Card)
    
    -- Perform the shuffle
    forM [0 .. len - 2] $ \i -> do
        j <- randomRIO (i, len - 1) -- Get a random index
        vi <- readArray arr i        -- Swap elements
        vj <- readArray arr j
        writeArray arr i vj
        writeArray arr j vi
    
    -- Convert the mutable array back to a pure list
    -- (The "elems" function from Data.Array does this, but this way works too)
    mapM (readArray arr) [0 .. len - 1]
    -- shuffle ini masih random, asumsi pure function gagal