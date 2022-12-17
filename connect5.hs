-- Connect5 game in Haskell

import Prelude
import Data.List (transpose, intercalate, tails)
import Control.Monad.RWS (All(getAll))
import Data.Bool (Bool)
import Data.Char (isDigit)

data BoardEntry = O | E | X deriving (Ord, Show)
type Row = [BoardEntry]
type Board = [Row]

data Tree a = Node a [Tree a]
                deriving (Show)

instance Eq BoardEntry where
    O == O = True
    X == X = True
    E == E = True
    _ == _ = False

-- Simply returns the grid representation which is in row order
rows :: Board -> Board
rows = id

showPlayer :: BoardEntry -> Char
showPlayer O = 'O'
showPlayer E = '.'
showPlayer X = 'X'

-- Finds the diagonals of the original grid representation which is in row order
diagonals :: Board -> Board
diagonals []       = repeat []
diagonals (xs:xss) = takeWhile (not . null) $
    zipWith (++) (map (:[]) xs ++ repeat [])
                 ([]:diagonals xss)


allDiagonals :: Board -> [Row]
allDiagonals xss = diagonals xss ++ diagonals (rotate90 xss)
    where rotate90 = reverse . transpose

-- Returns a column in the board given at a specific index
getCol :: Board -> Int -> [BoardEntry]
getCol board index = board !! index

gridSize :: Int
gridSize = 10


depth :: Int
depth = 5

-- Number of consecutive equivalent enteries needed to meet winning condition
win :: Int
win = 5

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

-- Checks if a list contains five consecutive enteries that are the same and not equal to empty
fiveConsecutive :: [BoardEntry] -> Bool
fiveConsecutive [] = False
fiveConsecutive (x:xs) = x /= E
                        && take 4 xs == replicate 4 x
                        || fiveConsecutive xs

-- Checks if a given board has a winner in each column vertically
validateWinVertical :: Board -> Bool
validateWinVertical = any fiveConsecutive

-- Checks if there are any five consecutive enteries horizontally
validateWinHorizontal :: Board -> Bool
validateWinHorizontal board = any fiveConsecutive $ transpose board

-- Checks if there are any five consecutive enteries diagonally
validateWinDiagonal :: Board -> Bool
validateWinDiagonal board = any fiveConsecutive $ allDiagonals board

validateWin :: Board -> Bool
validateWin board = validateWinDiagonal board
                    || validateWinHorizontal board
                    || validateWinVertical board

-- Checks if the move the player wants to make is valid
isValid :: Board -> Int -> Bool
isValid board index
                    | index >= gridSize || 0 > index = False
                    | head (map (!!index) board) /= E = False
                    | otherwise = True

-- Checks if a specific player has won
hasWon :: Board -> BoardEntry -> Bool
hasWon [] _ = False
hasWon board entry = any (containsWin entry) (getAllSubRows board) ||
                     any (containsWin entry) (getAllSubRows (transpose board)) ||
                     any (containsWin entry) (getAllSubRows (allDiagonals board))
                     where containsWin entry row = all (==entry) row

-- returns all sub rows for each row in the grid representation of the board
getAllSubRows :: Board -> [Row]
getAllSubRows = concatMap getSubRows

-- Returns every subset of consecutive elements in a list the size of the winning condition
getSubRows :: Row -> [Row]
getSubRows xs = [take win xs' | xs' <- tails xs, length xs' >= win]

-- Generate a list of next moves
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
next :: BoardEntry -> BoardEntry
next O = X
next X = O
next E = E

isGameOver :: Board -> Bool
isGameOver board = findWinner board /= E || isDraw board

--If player has won return X
--If computer has won return O
--If draw or inconclusive game return E
findWinner :: Board -> BoardEntry
findWinner board | hasWon board O = O
                 | hasWon board X = X
                 | otherwise = E

--Determines whose turn it is to play based on symetry of plays
turn :: Board -> BoardEntry
turn board = if os <= xs then O else X
         where
            os = length (filter (== O) $ concat board)
            xs = length (filter (== X) $ concat board)

-- Generates a gameSearchTree and prunes to a specfied depth
-- If a winning board is reached, add board as a leaf in the game tree
generateTree :: Int -> Board -> BoardEntry -> Tree Board
generateTree 0 board entry = Node board []
generateTree ct board entry = Node board [generateTree (depth-1) b (next entry) 
                                          | b <- possibleMoves board entry]


--label leaf nodes with winner
--propogate evaluation up the game tree
--If computer is playing take minimum of the children
--If player is playing take the maximum of the children
--output tree
minmax :: Tree Board -> Tree (Board, BoardEntry)
minmax (Node b []) = Node (b, findWinner b) []
minmax (Node b xss)
    | turn b == O = Node (b, min) xss'
    | turn b == X = Node (b, max) xss'
    | otherwise = Node (b, max) xss'
  where
    xss' = map minmax xss
    evals = [e' | Node (_,e') _ <- xss']
    min = minimum evals
    max = maximum evals

-- Given current board and the computer's entry, generate a gametree
-- Simulate minmax algorithm on the game tree
-- Randomly  pick one of the direct children of the root  node with the save evaluation
bestMove :: BoardEntry -> [Row] -> Board
bestMove entry board = head best_moves
      where
        searchTree = generateTree depth board entry
        Node (_, e) xss = minmax searchTree
        best_moves = [b' | Node (b', e') _ <- xss, e' == e]

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
run = play initEmptyBoard O

play :: Board -> BoardEntry -> IO()
play board entry = do
                   printBoard board
                   let prompt = "Please enter a column number from 0-9: "
                   userChoice <- getUserChoice prompt
                   let pBoard = makeMove userChoice entry board
                   printBoard pBoard
                   
                   if hasWon board entry || isDraw board 
                    then 
                      putStrLn "Player has won!"
                    else 
                      do 
                        let cBoard = bestMove (next entry) pBoard
                        if hasWon board (next entry) || isDraw board 
                          then 
                            putStrLn "Computer has won!"
                          else do
                            play cBoard entry 


getUserChoice :: String -> IO Int
getUserChoice prompt = do
                   putStr "Please enter a column number: "
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                      return (read xs)
                   else
                      do putStrLn "ERROR: Invalid number"
                         getUserChoice prompt
