# Tutorial for playing the Kabul Game

There are two games that is implemented here, that is the [CLI version](#cli-version) and [Web version](#web-version). Make sure Stack and Cabal is installed.

## CLI Version

### Installation

Go to the project directory

```bash
cd cabo-card-functional
```

After that, there are two options.

- Run the game instantly by entering the command

    ```bash
    stack build
    ```

    ```
    stack run
    ```

    This will compile the `Main.hs` file in [app](/app/Main.hs) folder.

- Run the game using `.exe`.

    ```bash
    stack build
    ```

    Locate the folder 

    ```text
    .\cabo-card-functional\.stack-work\install\....\bin
    ```

    There should be an executable files that you can run.

### Input/Output

1. Peek Phase

    After you run the command berfore, you terminal should something like this:

    ```text
    ====== GAME SIMULATION IN CLI ======


    ============================= TABLE =============================

    P0(0pts): [[]] [[]] [[]] [[]]      P1(0pts): [[]] [[]] [[]] [[]]

    P2(0pts): [[]] [[]] [[]] [[]]      P3(0pts): [[]] [[]] [[]] [[]]

    =================================================================
    Discard Pile: [Empty]
    Draw Deck   : 40
    Phase       : PEEK PHASE: Player in Turn 0
    =================================================================

    Your Cards (Player 0):
    [Card at 0]  [Card at 1]  [Card at 2]  [Card at 3]

    [INIT] initpeek <i1> <i2>: <input here>
    ```

    Now this is the "peek phase" of the game, on the terminal `<input here>` that is where you choose which cards to be peeked (between 0 to 3). Make sure dont pick the number outside of given card because it will give an error message. Your input should look something like `initpeek 1 3` that is peeking the card at 1 and 3 respectively. The output should look like this.

    ```text
    ...
    =================================================================
    Discard Pile: [Empty]
    Draw Deck   : 40
    Phase       : PEEK PHASE: Your card 0 (type 'finish' to proceed)
    =================================================================

    Your Cards (Player 0):
    [Card at 0]  [Card at 1]  [Card at 2]  [Card at 3]

    [!!! SECRET INFORMATION !!!] -> Inital Peek (P0): [1] Six of Clubs & [3] Jack of Spades

    [INFO] Type 'finish' to proceed:
    ```

    Now type "finish" to end your peek phase.

2. Draw and Discard

    After peeking phase of each players, now the output would look like this.

    ```text
    =================================================================
    Discard Pile: [Empty]
    Draw Deck   : 40
    Phase       : DrawPhase
    =================================================================

    Your Cards (Player 0):
    [Card at 0]  [Card at 1]  [Card at 2]  [Card at 3]

    CMD: draw | discard <i> | target <pid> <ids...> | skip:
    ```

    There are 3 types of command, that is `draw` , `discard` , `target` , and `skip`. From here wrong input would be automatically handled.

    At Draw Phase, you would draw one card from Draw Deck where now the card at 0 is the newly drawn card (you could see it as stacking to the right).

    ```text
    ...
    Your Cards (Player 0):
    [Card at 0]  [Card at 1]  [Card at 2]  [Card at 3]

    CMD: draw | discard <i> | target <pid> <ids...> | skip: draw
    [==== OK ====] 
    ...

    =================================================================
    Discard Pile: [Empty]
    Draw Deck   : 39
    Phase       : DiscardPhase
    =================================================================
    Your Cards (Player 0):
    [Card at 0]  [Card at 1]  [Card at 2]  [Card at 3]  [Card at 4]

    [!!! SECRET INFORMATION !!!] -> You've taken the card Ace of Hearts

    CMD: draw | discard <i> | target <pid> <ids...> | skip:
    ```

    Now at Discard Phase, you can discard one of you 5 cards in hand. Use the command `discard 1` and it will look like this.

    ```text
    ...
    CMD: draw | discard <i> | target <pid> <ids...> | skip: discard 1
    [==== OK ====] 

    ...

    =================================================================
    Discard Pile: Two of Hearts
    Draw Deck   : 39
    Phase       : Stack check (Rank: Two) -> Waiting for P0
    =================================================================
    ```

    That's the end of Draw and Discard Phase. After powerup phase, you can enter the command `finish` to give turn to the next player or you can enter `kabul` to end the game.

3. Stack Phase

    After Discard Phase ends, start the Stack Phase. Each player will be given an option to either stack the card in Discard Pile or just pass.

    ```text
    =================================================================
    Discard Pile: Two of Hearts
    Draw Deck   : 39
    Phase       : Stack check (Rank: Two) -> Waiting for P0
    =================================================================

    Your Cards (Player 0):
    [Card at 0]  [Card at 1]  [Card at 2]  [Card at 3]

    [== STACK ==] timpa <idx> / pass:
    ```

    - If you choose to pass, enter the command `pass` and stacking turn will be given to the next player. Make sure you see the Player info given by output.

    - If you choose to stack, enter the command `timpa i` where `i` is the card at hand. The game would recognize whether the card perfectly match Discard Pile.

    After every player used their turn, end of Stacking Phase.
  
4. Powerup Phase

    After discarding cards with powerups, you can choose `skip` to not trigger the powerups or you can use `target pid ids` where pid is the enemy id target and ids are the id for which card to be used on by the powerups. Each powerups can lead to different outputs, make sure to try any combinations. For example the SWITCH powerups targetting P0 at card index 0 (YOU) and 1 (ENEMY) gives

    ```
    ...
    =================================================================
    Discard Pile: Jack of Hearts
    Draw Deck   : 39
    Phase       : ResolvePowerup Switch
    =================================================================

    Your Cards (Player 0):
    [Card at 0]  [Card at 1]  [Card at 2]  [Card at 3]

    CMD: draw | discard <i> | target <pid> <ids...> | skip: target 0 0 1

    ...
    =================================================================
    Discard Pile: Jack of Hearts
    Draw Deck   : 39
    Phase       : End of Turn -> type 'kabul' atau 'finish'
    =================================================================

    Your Cards (Player 0):
    [Card at 0]  [Card at 1]  [Card at 2]  [Card at 3]

    [!!! SECRET INFORMATION !!!] -> Switch Performed successfully.

    [END] kabul / finish: finish

    ```



## Web version

### Installation

Go to the project directory

```bash
cd cabo-card-functional
```

Now, run the command

```bash
stack test
```

There should be an output in terminal that looks like this

```text
====== KABUL SERVER READY (PORT 3000) ======
Setting phasers to stun... (port 3000) (ctrl-c to quit)
```

Go to your browser and then type [http://localhost:3000/](http://localhost:3000/). The game then can be played there. 