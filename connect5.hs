import Data.List (transpose, intercalate)

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

rotate90 :: [[a]] -> [[a]]
rotate90 = reverse . transpose

allDiagonals :: Board -> [Row]
allDiagonals xss = diagonals xss ++ diagonals (rotate90 xss)

-- Returns a column in the board given at a specific index
getCol :: Board -> Int -> [BoardEntry]
getCol board index = board !! index

-- 15 x 15 grid/board size
gridSize :: Int
gridSize = 10

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

isValidMove :: Board -> Int -> Bool
isValidMove board index = index <= gridSize -1
                          && index >= 0
                          && not (any ((/=E) . (!!index)) board)

possibleMoves :: Board -> BoardEntry -> [Board]
possibleMoves board entry = [makeMove i entry board | i <- [0..(gridSize-1)] , isValidMove board i]

oppositeEntry :: BoardEntry -> BoardEntry
oppositeEntry O = X
oppositeEntry X = O
oppositeEntry E = E

printBoard :: Board -> IO ()
printBoard board = showBoard $ transpose board

showBoard :: Board -> IO ()
showBoard b = putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
              where
                showRow = map showPlayer
                line = replicate gridSize '-'
                nums = take gridSize ['0'..]
