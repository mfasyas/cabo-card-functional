# Cabo Card Game
This repository is a project to complete the functional programming course.

The game is based on an actual board game [Cabo](https://en.wikipedia.org/wiki/Cabo_(game)) with some modification of some [rules](https://gist.github.com/bag-man/803c4a85ab3a07c2cc5d036738ec8802) using playing cards instead of an actual custom set of deck of cards.

As per November 24th, 2025 thisproject has not yet been completed.

The target of this project is to use the approach of functional programming in to the game as much as possible. The program's backend is written in [Haskell](https://img.shields.io/badge/Built_with-Haskell-purple) with [Frontend](https://img.shields.io/badge/Frontend-HTML%2FCSS%2FJS-orange) as the main tool for frontend.

## Dependencies
Project is build using [Stack](https://docs.haskellstack.org/en/stable/) with

- **[scotty](https://hackage.haskell.org/package/scotty):** 

- **[wai-cors](https://hackage.haskell.org/package/wai-cors)** 

- **[aeson](https://hackage.haskell.org/package/aeson):**

- **[stm (Software Transactional Memory)](https://hackage.haskell.org/package/stm)

- **[mtl](https://hackage.haskell.org/package/mtl):**

More specifically you can see the file `package.yaml` in the repository for more information

## Clone and Running the Program

1. Clone Repository 

    ```bash
    git clone [https://github.com/mfasyas/cabo-card-functional.git](https://github.com/mfasyas/cabo-card-functional.git)
    cd cabo-card-functional
    ```

2. Run the Program

    In order to run the program, make sure Stack is already installed on your computer. After installing Stack and [Cabal](https://www.haskell.org/cabal/) all you got to do is run this line in terminal.

    ```bash
    stack build
    ```
    
    - For running main program do

        ```bash
        stack run
        ```

    For running test do

        ```bash
        stack test
        ```

## To Do in the Future

- Make the game multiplayer by using [Websocket](https://hackage.haskell.org/package/websockets) 

- Creating UI that fits best

- More flexible feature.

## Project Structure

```text
cabo-card-functional/
├── app
│    └── Main         # Main Program
├── src               # Module to be imported
│    ├── temp         # Temporary files after update
│    ├── Action
│    ├── Card
│    ├── GameEngine
│    ├── GameRules
│    ├── GameStates
│    ├── Player
│    └── Powerups
├── test              # Testing file
│    └── TestMain
└── misc ...          # Misc files
```

## Explanation

A bit of explanation, this repository is divided into three branches that is `main` as the "best update" possible branch, `add-feature` for feature testing, and `user-interface` for UI/UX testing.

For the functional programming paradigm that is used here is mainly about switching imperative code (traces back to some commits) into a more declarative code that is easily maintained and much more .