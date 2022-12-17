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

-- 15 x 15 grid/board size
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

-- checks if a list contains five consecutive enteries that are the same and not equal to empty
fiveConsecutive :: [BoardEntry] -> Bool
fiveConsecutive [] = False
fiveConsecutive (x:xs) = x /= E
                        && take 4 xs == replicate 4 x
                        || fiveConsecutive xs

-- checks if a given board has a winner in each column vertically
validateWinVertical :: Board -> Bool
validateWinVertical = any fiveConsecutive

-- checks if there are any five consecutive enteries horizontally
validateWinHorizontal :: Board -> Bool
validateWinHorizontal board = any fiveConsecutive $ transpose board

-- checks if there are any five consecutive enteries diagonally
validateWinDiagonal :: Board -> Bool
validateWinDiagonal board = any fiveConsecutive $ allDiagonals board

validateWin :: Board -> Bool
validateWin board = validateWinDiagonal board
                    || validateWinHorizontal board
                    || validateWinVertical board

-- checks if the move the player wants to make is valid
isValid :: Board -> Int -> Bool
isValid board index 
                    | index < gridSize || 0 <= index = False        
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
possibleMoves board entry | findWinner board /= E = []
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

--If player has won return X
--If computer has won return O
--If draw or inconclusive game return E
findWinner :: Board -> BoardEntry
findWinner board | hasWon board O = O
                 | hasWon board X = X
                 | otherwise = E

-- Generates a gameSearchTree and prunes to a specfied depth
-- If a winning board is reached, add board as a leaf in the game tree
generateTree :: Int -> Board -> BoardEntry -> Tree (Board, Int)
generateTree 0 board entry = Node (board, depth) []
generateTree ct board entry = Node (board, depth - ct) [generateTree (depth-1) b (next entry) | b <- possibleMoves board entry]

                                
--label leaf nodes with winner
--propogate evaluation up the game tree
--If computer is playing take minimum of the children
--If player is playing take the maximum of the children
--output tree

minmax :: BoardEntry -> Tree (Board,Int) -> Tree (Board,BoardEntry)
minmax p (Node (b,d) []) = Node (b, findWinner b) []
minmax p (Node (b,d) xss) = Node (b, bestEval) xss'
  where
    xss' = map (minmax (next p)) xss
    evals = [winnerLabel | Node (_,winnerLabel) _ <- xss']
    take_max_or_min  = if p==X then maximum else minimum
    bestEval = take_max_or_min evals

-- Given current board and the computer's entry, generate a gametree
-- Simulate minmax algorithm on the game  tree
-- Randomly  pick one of the direct children of the root  node with the save evaluation
bestMove :: BoardEntry -> [Row] -> Board
bestMove entry board = do
  let tree = generateTree depth board entry
      Node (_, eval) xss = minmax entry tree
      possible_moves = [b' | Node (b', e') _ <- xss, e' == eval]  
      return (head possible_moves)

-- given a gametree labled with 
-- prints the grid to the terminal
printBoard :: Board -> IO ()
printBoard board = printBoard' $ transpose board
  where  printBoard' b = putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
              where
                showRow = map showPlayer
                line = replicate gridSize '-'
                nums = take gridSize ['0'..]


-- play :: Board -> BoardEntry -> IO()
-- play board entry = do 
--                    userChoice <- getUserChoice board
--                    pBoard <- makeMove userChoice (next entry) board


getUserChoice :: String -> IO Int
getUserChoice prompt = do 
                   putStr "Please enter a column number: "
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                      return (read xs)
                   else
                      do putStrLn "ERROR: Invalid number"
                         getUserChoice prompt
