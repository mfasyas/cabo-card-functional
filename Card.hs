module Card where
import Foreign (Storable(peek))
import GHC.Base (build)
import Data.List (intercalate)

data Suit = Hearts | Diamonds | Clubs | Spades | RedJoker | BlackJoker
  deriving (Show, Eq)

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Joker
  deriving (Show, Eq, Ord, Enum, Bounded)

data Powerup = Normal | Peek | Trade
  deriving (Show, Eq)

data Card = Card
  { rank    :: Rank
  , suit    :: Suit
  , powerup :: Powerup
  } deriving (Eq)

cardValue :: Card -> Int
cardValue (Card r _ _) = case r of
  Ace    -> 1
  Two    -> 2
  Three  -> 3
  Four   -> 4
  Five   -> 5
  Six    -> 6
  Seven  -> 7
  Eight  -> 8
  Nine   -> 9
  Ten    -> 10
  Jack   -> 10
  Queen  -> 10
  King   -> 10
  Joker  -> 15


type Deck = [Card]
type Pile = [Card]

newtype Hand = Hand [Card] deriving (Eq)
instance Show Hand where
    show (Hand hs) = "[" ++ intercalate ", " (map showCardRS hs) ++ "]"

instance Show Card where
    show (Card r s p) = show r ++ " of " ++ show s ++ " " ++ show p

-- =============================================================================
-- Lists of functions
addCards :: Card -> Card -> Int
addCards a b = cardValue a + cardValue b -- this part could be not useful

handScore :: Hand -> Int
handScore (Hand hs) = sum (map cardValue hs)

buildDeck :: Deck
buildDeck = normalCards ++ jokerCards
  where
    normalCards = [ Card r s (powerFor r) | s <- [Hearts, Diamonds, Clubs, Spades], r <- [Ace .. King] ]
        where 
            powerFor :: Rank -> Powerup
            powerFor Jack  = Trade
            powerFor Queen = Trade
            powerFor King  = Trade
            powerFor _     = Normal
    jokerCards  = [ Card Joker RedJoker   Normal | _ <- [1..2] ] ++ [ Card Joker BlackJoker Normal | _ <- [1..2] ]

showCardRS :: Card -> String
showCardRS (Card r s _) = show r ++ " of " ++ show s

-- This function takes a deck and returns:
-- 1. The card on top
-- 2. The *rest* of the deck
drawCard :: Deck -> (Card, Deck)
drawCard [] = error "Cannot draw from an empty deck!"
drawCard (topCard : restOfDeck) = (topCard, restOfDeck)

-- =============================================================================
-- Card Test Data
testCard1 :: Card
testCard1 = Card { rank = Ace, suit = Hearts, powerup = Peek }
-- =============================================================================
-- Deck Test
deck1 :: [Card]
deck1 = take 10 buildDeck

deck2 :: [Card]
deck2 = filter (\c -> rank c `elem` [Jack, Queen, King, Joker]) buildDeck
-- =============================================================================