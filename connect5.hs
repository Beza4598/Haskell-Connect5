-- Connect5 game in Haskell
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Prelude
import Data.List (transpose, tails)
import Data.Bool ()
import Data.Char (isDigit)
import System.Random ( randomRIO ) -- cabal install --lib random
import System.IO.Unsafe ( unsafePerformIO )
import Control.DeepSeq (NFData (rnf))
import Control.Parallel.Strategies
    ( parList, rdeepseq, using ) 
import Control.Monad.Par ( runPar, parMap ) -- cabal install --lib monad-par
import Data.Tree

data BoardEntry = O | E | X deriving (Ord, Show)
type Column = [BoardEntry]
type Board = [Column]

instance NFData BoardEntry where
  rnf x = x `seq` ()

instance Eq BoardEntry where
    (==) :: BoardEntry -> BoardEntry -> Bool
    O == O = True
    X == X = True
    E == E = True
    _ == _ = False


mode :: [Char]
mode = "par2"

ifMode :: [Char] -> BoardEntry -> Tree Board -> Tree (Board, BoardEntry)
ifMode "seq" = minmax
ifMode "par1" = minmaxPar
ifMode "par2" = minmaxParTwo


showPlayer :: BoardEntry -> Char
showPlayer O = 'O'
showPlayer E = '.'
showPlayer X = 'X'

-- Returns a column in the board given at a specific index
getCol :: Board -> Int -> [BoardEntry]
getCol board index = board !! index

gridSize :: Int
gridSize = 7

depth :: Int
depth = 6

-- Number of consecutive equivalent enteries needed to meet winning condition
win :: Int
win = 4

-- Game mechanics --

-- Initializes an empty board
initEmptyBoard :: Board
initEmptyBoard = replicate gridSize $ replicate gridSize E

-- Drops an entry into the lowest unnocupied position of a column
dropEntry :: [BoardEntry] -> BoardEntry -> [BoardEntry]
dropEntry column entry =
            let filled_entries = filter (/=E) column
                empty_entries =  gridSize - length filled_entries
            in  if empty_entries == 0 then
                 column
                else
                replicate (empty_entries - 1) E ++ [entry] ++ filled_entries

-- Adds an new entry into the board and updates the board
makeMove :: Int -> BoardEntry -> Board -> Board
makeMove i entry board =  take i board ++ [dropEntry (getCol board i) entry] ++ drop (i + 1) board

-- Checks if the move the player wants to make is valid
isValid :: Board -> Int -> Bool
isValid board index
                    | index >= gridSize || 0 > index = False
                    | head (board !! index) /= E = False
                    | otherwise = True

diagonals :: Board -> Board
diagonals []       = repeat []
diagonals (xs:xss) = takeWhile (not . null) $
    zipWith (++) (map (:[]) xs ++ repeat [])
                 ([]:diagonals xss)

-- Finds the diagonals and anti-diagonals of the original grid representation 
allDiagonals :: Board -> [Column]
allDiagonals xss = diagonals (transpose xss) ++ diagonals (rotate90 xss)
    where rotate90 = reverse


-- Checks if a specific player has won
hasWon :: Board -> BoardEntry -> Bool
hasWon board entry = any (all (==entry)) (rows ++ cols ++ diags)
  where
    rows = winChunks board
    cols = winChunks (transpose board)
    diags = winChunks (allDiagonals board)

-- returns all sub rows for each row in the grid representation of the board
winChunks :: Board -> [Column]
winChunks = concatMap slidingWindow

-- Returns every subset of consecutive elements in a list the size of the winning condition
slidingWindow :: Column -> [Column]
slidingWindow xs = [take win xs' | xs' <- tails xs, length xs' >= win]

-- Generate a list of nextEntry moves
possibleMoves :: Board -> BoardEntry -> [Board]
possibleMoves board entry | isGameOver board = []
                          | otherwise = [makeMove i entry board | i <- [0..(gridSize-1)] , isValid board i]

-- Check if board is full
isFull :: Board -> Bool
isFull = notElem E . concat

-- Check if board is full and there are no empty spaces
isDraw :: Board -> Bool
isDraw board = isFull board && findWinner board == E

-- Outputs the nextplayer given the last player
nextEntry :: BoardEntry -> BoardEntry
nextEntry O = X
nextEntry X = O
nextEntry E = E

isGameOver :: Board -> Bool
isGameOver board = findWinner board /= E || isDraw board

--If player has won return X
--If computer has won return O
--If draw or inconclusive game return E
findWinner :: Board -> BoardEntry
findWinner board | hasWon board O = O
                 | hasWon board X = X
                 | otherwise = E



-- Game logic (AI) ---

-- Generates a gameSearchTree and prunes to a specfied depth
-- If a winning board is reached, add board as a leaf in the game tree
generateTree :: Board -> BoardEntry -> Tree Board
generateTree board entry = Node board [generateTree b (nextEntry entry) | b <- possibleMoves board entry]

-- static depth pruning for gametree
prune :: Int -> Tree Board -> Tree Board
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

-- X is maximizing
-- O is minimizing
isMaximizing :: BoardEntry -> Bool
isMaximizing entry
            | entry == O = False
            | entry == X = True

--naive minimax algorithm
--labels leaf nodes with winner
--propogates evaluation up the game tree
--If computer is playing take minimum of the children
--If player is playing take the maximum of the children
--output tree
minmax ::  BoardEntry -> Tree Board -> Tree (Board, BoardEntry)
minmax _ (Node b []) = Node (b, findWinner b) []
minmax entry (Node b xss) = Node (b, evaluate evals) xss'
        where
          xss' = map (minmax (nextEntry entry)) xss
          evals = [e' | Node (_,e') _ <- xss']
          evaluate = if isMaximizing entry then maximum else minimum

-- Parallelizes by using a strategy (parList rdeepseq) to evaluate the tree in chunks equal to the 
-- Number of possible moves from the root tree
minmaxPar :: BoardEntry -> Tree Board -> Tree (Board, BoardEntry)
minmaxPar _ (Node b []) = Node (b, findWinner b) []
minmaxPar entry (Node b xss) = Node (b, evaluate evals) xss'
        where
          xss' = map (minmax (nextEntry entry)) xss `using` parList rdeepseq
          evals = [e' | Node (_,e') _ <- xss']
          evaluate = if isMaximizing entry then maximum else minimum


-- Parallelizes by applying parMap to evaluate the tree in chunks equal to the 
-- Number of possible moves from the root tree
minmaxParTwo :: BoardEntry -> Tree Board -> Tree (Board, BoardEntry)
minmaxParTwo _ (Node b []) = runPar $ return $ Node (b, findWinner b) []
minmaxParTwo entry (Node b xss) = runPar $ do
        xss' <- parMap (minmax (nextEntry entry)) xss
        let evals = [p | Node (_, p) _ <- xss']
        let evaluate = if isMaximizing entry then maximum else minimum
        return $ Node (b, evaluate evals) xss'

-- Given current board and the computer's entry, generate a gametree
-- Simulate minmax algorithm on the game tree
-- Randomly  pick one of the direct children of the root  node with the save evaluation
bestMove :: BoardEntry -> [Column] -> Board
bestMove entry board = best_moves !! random
      where
        gametree = prune depth (generateTree board entry)
        Node (_, best) xss = ifMode mode entry gametree
        best_moves = [b' | Node (b', e') _ <- xss, e' == best]
        random  = unsafePerformIO (randomRIO (0,length best_moves -1))


-- Utility Functions --

-- given a gametree labled with 
-- prints the grid to the terminal
printBoard :: Board -> IO ()
printBoard board = printBoard' $ transpose board
  where  printBoard' b = putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
              where
                showRow = map showPlayer
                line = replicate gridSize '-'
                nums = take gridSize ['0'..]

run :: IO()
run = gameLoop initEmptyBoard O

gameLoop :: Board -> BoardEntry -> IO()
gameLoop board entry = do
                   let prompt = "Please enter a column number from 0-6: \n"
                   userChoice <- getUserChoice board prompt
                   let pBoard = makeMove userChoice entry board
                   
                   printBoard pBoard

                   if isGameOver pBoard
                    then do 
                      putStrLn "Player has won!\n"
                    else
                      do
                        putStrLn "Computer is processing next move ...\n"
                        let cBoard = bestMove (nextEntry entry) pBoard
                        printBoard cBoard
                        if isGameOver cBoard
                          then do
                            putStrLn "Computer has won!\n"
                          else do
                            gameLoop cBoard entry

simulate :: Board -> BoardEntry -> IO()
simulate board entry = do
                   printBoard board
                   let cBoard_one = bestMove entry board
                   printBoard cBoard_one

                   if isGameOver cBoard_one
                    then
                      putStrLn "Computer 1 has won!"
                    else
                      do
                        let cBoard = bestMove (nextEntry entry) cBoard_one
                        if isGameOver cBoard
                          then do
                            printBoard cBoard
                            putStrLn "Computer 2 has won!"
                          else do
                            simulate cBoard entry

getUserChoice :: Board -> String -> IO Int
getUserChoice board prompt = do
                   putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs && isValid board (read xs) then
                      return (read xs)
                   else
                      do putStrLn "ERROR: Invalid number"
                         getUserChoice board prompt

--start program when executed
main :: IO()
main = run