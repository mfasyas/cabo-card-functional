# Rules of Kabul

This rule is the modified version of [kabul](https://gist.github.com/bag-man/803c4a85ab3a07c2cc5d036738ec8802)

## Objective

The game is played by 4 players. The main objective of the game is to have the lowest possible total values of cards in hand compared to other players. In other words point scoring is based on the values of your cards in hand.

If the you the lowest sum of values of card, the player gets 3 points. If you have the second lowest, the you get 2 points. Third lowest gets 1 points. The last place gets 0 points.

## Setup

1. A full deck of playing cards (including jokers) is shuffled. Each players is dealt with 4 cards face down.

2. Arrange the card forming 1x4 grid (see [Tutorial](/more_readme/TUTORIAL.MD)). The rest of deck as draw deck is then placed in the middle of the table face down, and then later discard deck is placed face up next to it as the discard pile.

## Gameplay

1. On the beginning of the game (Peek Phase), player can peek 2 out of 4 cards in their hands. Just make sure other doesnt know the cards. After confirming those 2 cards, it is then turned face down again. After this, the first player takes turn.

2. On a player's turn, player must draw a card first from the draw deck and open it. After that they can choose whether to discard the drawn card OR discard 1 of the 4 cards in hand. If the card in hand is discarded, make sure the newly drawn card replaces the card (in the implemented game, the drawn card is placed like a stack). 

3. Now after discarding a card, a card may contain some [powerups](#powerups). A player choose to trigger the powerups or just skip it as a normal card would.

4. After using the powerups, player can choose to "kabul" and ends them entirely. The turn is finished after this.

5. When a player discards a card, any player (including the palyer in turn) may "stack" the card that is discarding a card that has the same rank with one of their hand.

    - If the ranks match, the card stays in discard pile.

    - Otherwise, the card must return to the hand and draw one card from draw deck.

6. To end the game there are two possibilities.

- A player says "kabul", then the round ends and all players should compare the values of the cards.

- The draw deck is empty

## Cards

| Card         | Powerup / Function | Value |
|--------------|--------------------|--------|
| 1–6          | Normal             | normal value |
| 7 / 8        | PeekSelf           | normal value |
| 9            | PeekOpponent       | normal value |
| 10           | PeekSO             | normal value |
| Jack / Queen | Switch             | 11 |
| Red King     | Normal             | 0 |
| Black King   | PeekSwitch         | 12 |
| Black Joker  | PeekDouble         | 15 |
| Red Joker    | Normal             | -1 |

Normal value means the card has the exact sam value as their ranks.

## Powerups

- **PeekSelf** — Reveal 1 card from your own hand.  
- **PeekOpponent** — Reveal 1 card from the opponent’s hand.  
- **PeekSO** — Reveal 1 card from your hand and 1 card from the opponent’s hand.  
- **Switch** — Swap 1 card with the opponent without revealing it.  
- **PeekSwitch** — Reveal 1 card from your hand and the opponent’s, then swap 1 card.  
- **PeekDouble** — reveal 2 cards from your hand and 2 cards from the opponent’s hand.