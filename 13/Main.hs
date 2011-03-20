module Main where

import System.Console.Readline
import Data.List (elemIndex)

-- GameState = (remainingMoves, currentPlayer)
-- 0 == human
-- 1 == computer
type GameState = (Integer, Integer)
data GameTree = GameTree GameState [GameTree]
    deriving Show

otherPlayer :: Integer -> Integer
otherPlayer p = if p == 0 then 1 else 0

nextMoves :: GameState -> [GameState]
nextMoves (moves, player) = [(m, otherPlayer player) | m <- reverse [(max 0 (moves - 3)) .. (moves - 1)]]

makeTree :: GameState -> GameTree
makeTree s = GameTree s (map makeTree (nextMoves s))

-- min/max rating algorithm, assume perfect game of both players
-- returns number of the winning player (0: player, 1: computer)
rateTree :: GameTree -> Integer
rateTree (GameTree (0, p) _) = p
rateTree (GameTree (_, p) states) =
    if( p == 0 ) then
        (foldl min 1 $ map rateTree states)
    else
        (foldl max 0 $ map rateTree states)

getPlayerName p = if p == 0 then "Player" else "Computer"

printGameState :: GameState -> String
printGameState (moves, player) =
    "remaining: " ++ (show moves)
    ++ " "
    ++ (concat $ take (fromInteger moves) $ repeat "|")

-- ask the player about the number of matches to pick
playerMove :: GameTree -> IO Integer
playerMove (GameTree state nextMoves) = do
    putStrLn $ printGameState state
    -- putStrLn $ show $ map rateTree nextMoves -- debug
    putStrLn $ "please input a number from 1 to " ++ (show $ length nextMoves) ++ " (0 to exit)"
    maybeLine <- readline "> "
    case maybeLine of 
        Nothing   -> return 0
        Just "0"  -> return 0
        Just line -> do
                     let num = read line
                     if ( num >= 1 && num <= fromIntegral (length nextMoves) )
                         then return num
                         else do
                              putStrLn "invalid input, try again"
                              playerMove (GameTree state nextMoves)

-- let the computer determine the number of matches to pick
computerMove :: GameTree -> IO Integer
computerMove (GameTree state nextMoves) = do
    let num = case elemIndex 1 (map rateTree nextMoves) of
            Nothing -> 1 -- if there is no move where computer wins, pick 1
            Just n -> n + 1 -- index + 1 = the number of matches to pick
    -- putStrLn $ show $ map rateTree nextMoves -- debug
    putStrLn $ "computer picks " ++ (show num)
    return $ fromIntegral num

game :: GameTree -> IO ()
game (GameTree state nextMoves) =
    if fst state == 0 then do
        putStrLn $ (getPlayerName $ snd state) ++ " wins."
        return ()
    else do
        nextMove <- (if snd state == 0 then playerMove (GameTree state nextMoves) else computerMove $ GameTree state nextMoves)
        if( nextMove == 0 )
            then return ()
            else game $ head $ drop ((fromInteger nextMove) - 1) nextMoves
    
main = do
    game $ makeTree (13, 0)
