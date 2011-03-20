# 13

Little game with matches for two players.

## Rules

There are thirteen matches on the table. In each round, the player has to pick
either one, two or three matches.  The player who has to pick the last match
loses.

## About this program

This program implements the rules of the game, together with a game AI. I did
it because I wanted to experiment with game trees and tree evaluation
algorithms. The game tree is small enough for exhaustive search without any
heuristics.

With 13 matches, assuming perfect players, the player who starts cannot win.

## How to play

1. install libreadline-dev

        $ sudo aptitude install libreadline6-dev

2. install readline haskell bindings

        $ cabal install readline

3. build the program

        $ ghc --make Main.hs

4. launch the program

        $ ./Main

## Screenshot

    remaining: 13 |||||||||||||
    please input a number from 1 to 3 (0 to exit)
    > 3
    computer picks 1
    remaining: 9 |||||||||
    please input a number from 1 to 3 (0 to exit)
    > 2
    computer picks 2
    remaining: 5 |||||
    please input a number from 1 to 3 (0 to exit)
    > 3
    computer picks 1
    remaining: 1 |
    please input a number from 1 to 1 (0 to exit)
    > 1
    Computer wins.
