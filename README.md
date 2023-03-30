### Haskell-Connect5

This repository contains a parallel implementation of the classic Connect-5 game, written in Haskell. The goal of this project is to leverage the power of parallelism to speed up the game's computation and make it more efficient.

The implementation is based on the Minimax algorithm, a popular game-playing algorithm that uses a depth-first search with alpha-beta pruning to maximize the minimum possible gain. By parallelizing this algorithm, we can take advantage of the multi-core processors in modern machines to significantly reduce the time it takes to compute a move.

This implementation supports both human-vs-human and human-vs-AI gameplay modes, with the ability to set the depth of the search algorithm and the number of threads used for parallel computation.

To get started, simply clone this repository and run the connect5 executable. The game is played in the terminal, with the board represented as a grid of symbols. Follow the prompts to choose your game mode and set the difficulty level.

We hope you enjoy playing and exploring this parallel implementation of Connect-5 in Haskell!
