import Data.List (transpose, intercalate)

data BoardEntry = O | E | X deriving (Ord)
type Row = [BoardEntry]
type Board = [Row]

data Tree a = Node a [Tree a]
                deriving (Show)


instance Show BoardEntry where
    show O = "O  "
    show X = "X  "
    show E = ".  "

instance Eq BoardEntry where
    O == O = True
    X == X = True
    E == E = True
    _ == _ = False

-- Simply returns the grid representation which is in row order

rows :: Board -> Board
rows = transpose

-- Transforms the matrix from row order to column order
cols :: Board -> Board
cols = id

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
getCol board index = cols board !! index

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
            let filled_entries = filter (==E) column
                empty_entries =  15 - length filled_entries
            in replicate (empty_entries - 1) E ++ [entry] ++ filled_entries

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

printBoard :: Board -> IO ()
printBoard board = print $ transpose board

possibleMoves :: Board -> BoardEntry -> [Board]
possibleMoves board entry = [ makeMove i entry board| col <- board , i <- [1..gridSize]]

oppositeEntry :: BoardEntry -> BoardEntry
oppositeEntry O = X
oppositeEntry X = O
oppositeEntry E = E