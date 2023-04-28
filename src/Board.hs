module Board (
    emptyBoard,
    setSquare,
    getSquare,
    printBoard,
    placePiece,
    removePiece,
    countPieces,
    Board,
    Square(..),
    Piece(..),
    PieceType(..),
    Position(..)
) where
-- | Hello 
import Data.List (intercalate)
import Data.Char(toLower)

data PieceType = Assassin | Skirmisher | Defender deriving (Eq)
data Piece = Piece { pieceType :: PieceType, isEnemy :: Bool } deriving (Eq)

data Position = Position {row :: Int, col :: Int} deriving (Eq)

data Square = EmptySquare | OccupiedSquare PieceType Bool deriving (Eq)
type Board = [[Square]]

-- | Creates an empty board
emptyBoard :: Int -> Int -> Board
emptyBoard width height = replicate height (replicate width EmptySquare)

-- | Changes the square on a given coordinates and returns a updated board with the new square
setSquare :: Board -> (Int, Int) -> Square -> Board
setSquare board (x, y) square = take y board
                              ++ [take x row ++ [square] ++ drop (x + 1) row]
                              ++ drop (y + 1) board
    where row = board !! y

-- | Takes a board and gives us the square on the given coordinates
getSquare :: Board -> Position -> Square
getSquare board (Position row col) = board !! row !! col

emptySquares :: Board -> [Position]
emptySquares board = [Position x y | x <- [0..width-1], y <- [0..height-1], getSquare board (Position x y) == EmptySquare && y >= height `div` 2]
  where
    width = length (head board)
    height = length board



-- | Pieces are represented by the first letter of their type, and differentiated by lower and uppercase letters depending on if they are enemy or ally pieces.
showPieceType :: PieceType -> Char
showPieceType Assassin = 'A'
showPieceType Defender = 'D'
showPieceType Skirmisher = 'S'

showPiece :: Piece -> Char
showPiece (Piece pt False) = showPieceType pt
showPiece (Piece pt True) = toLower (showPieceType pt)


-- | Places a piece on a given coordinate on the board.
placePiece :: Board -> (Int, Int) -> (PieceType, Bool) -> Board
placePiece board (x, y) (pieceType, isEnemy) = setSquare board (x, y) (OccupiedSquare pieceType isEnemy)

-- | Removes a piece on a given position and replaces it with an empty square
removePiece :: Board -> (Int, Int) -> Board
removePiece board (x, y) = setSquare board (x, y) EmptySquare

-- | We flatten the board into a single list of squares
-- | Then we filter the list by checking if each square in the list is an Occupiedsquare with a piece represented by an uppercase letter.
-- | We then return the lenght of the filtered list to get how many pieces of the type we want there is.
countPieces :: Board -> PieceType -> Int
countPieces board pieceType = length $ filter isMatch (concat board)
  where
    isMatch (OccupiedSquare pt False) = pt == pieceType
    isMatch _ = False


-- | Represents each square depending on if it is empty or occupied, and if the square has an ally or enemy piece.
showSquare :: Square -> Char
showSquare EmptySquare = '.'
showSquare (OccupiedSquare pieceType isEnemy) = if isEnemy then toLower (showPieceType pieceType) else showPieceType pieceType

-- | Maps over each square to find their character representation using showSquare. It then adds each character representation to a list
showRow :: [Square] -> String
showRow = intercalate "" . map (return . showSquare)

showBoard :: Board -> String
showBoard = unlines . map showRow

printBoard :: Board -> IO ()
printBoard = putStrLn . showBoard

