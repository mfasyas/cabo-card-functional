# Cabo Card Game
This repository is a project to complete the functional programming course.

The game is based on an actual board game [Cabo](https://en.wikipedia.org/wiki/Cabo_(game)) with some modification of some [rules](https://gist.github.com/bag-man/803c4a85ab3a07c2cc5d036738ec8802) using playing cards instead of an actual custom set of deck of cards. You can check the custom rule in [Rules](more_readme/RULES.md).

The target of this project is to use the approach of functional programming in to the game as much as possible. The program's backend is written in [Haskell](https://www.haskell.org/) with HTML as the main tool for frontend.

The project is build inside [Stack](https://docs.haskellstack.org/en/stable/) and [Cabal](https://www.haskell.org/cabal/) environment with Scotty as the main deployment.

The game is played by hotseat that is, players took turn to play the game in one device. For now, the game is only playable in PC (or laptop) browser in [here](tutorial-browser) or using simple CLI in [here](tutorial-cli). For future update check [updates](this-is-for-fixes.com) for more information.

- See [Tutorial](./more_readme/TUTORIAL.MD) for guides to play the game.

- See [Functional](./more_readme/FUNCTIONAL.md) for the implementation of functional programming aspects as well compared to the previous IO side-effects and imperative approach.

## Features

- Intuitive gameplay coupled with good handling of mis-inputs and validations.

- Flexible powerup mechanism for strategy players coupled with information handling.

- Kabul (end game) and scoring system.

- Multiplayer hotseat gameplay

- Stack cards in timpa.

- Declarative Rule Systems for scalability of features.

## Dependencies
Project is built using

- **[Scotty](https://github.com/scotty-web/scotty)** 

- **[Cross-Origin Resource Sharing - Web Application Interface](https://hackage.haskell.org/package/wai-cors)** 

- **[Aeson Parser](https://hackage.haskell.org/package/aeson)**

- **[Software Transactional Memory](https://hackage.haskell.org/package/stm)**

- **[Monad Class for Transformers](https://hackage.haskell.org/package/mtl)**

## Clone and Installations

1. Clone Repository 

    ```bash
    git clone [https://github.com/mfasyas/cabo-card-functional.git](https://github.com/mfasyas/cabo-card-functional.git)
    cd cabo-card-functional
    ```

2. Run the Program

    In order to run the program, make sure Stack and Cabal is already installed on your computer. After that, all you got to do is run this line in terminal.

    ```bash
    stack build
    ```
    
    For running main CLI game, run

    ```bash
    stack run
    ```

    Go to [Tutorial](./more_readme/TUTORIAL.MD) for more information.

## Project Structure

```text
cabo-card-functional/
├── app
│    └── Main         # File for CLI game
├── src               # Modules to be imported
│    ├── Card
│    ├── GameEngine
│    ├── GameRules
│    ├── GameStates
│    └── Player
├── test              # File for Web Game -- still in testing
│    └── TestMain
├── index.html        # HTML file
├── script.js         # Javascript file
├── styles.css        # Game style
└── misc ...          # Misc files
```

## Git Branches

The repository is divided into 4 branches. 

* Branch `main` is the actual program that you can use to play the game. Branch

* Branch `add-feature` for testing features implemented in the game, still purely on CLI.

* Branch `user-interface` for testing the game using HTML. This branch mainly used for integrating the game to the frontend.

* Branch `non-drs` is the first generation code. The game is implemented in imperative first, then using refactored to the current code in `main`.