module Board (
    assasinStats,
    skirmisherStats,
    defenderStats,
    emptyBoard,
    setSquare,
    getSquare,
    emptySquares,
    printBoard,
    placePiece,
    removePiece,
    countPieces,
    showPieceType,
    showPiece,
    showSquare,
    showRow,
    showBoard,
    doAttack,
    GameBoard,
    Square(..),
    Piece(..),
    PieceType(..),
    PieceStats(..),
    Position(..)
) where

import Data.List (intercalate)
import Data.Char(toLower)

data PieceType = Assassin | Skirmisher | Defender deriving (Eq)
data Piece = Piece { pieceType :: PieceType, isEnemy :: Bool, pieceStats :: PieceStats } deriving (Eq)
data PieceStats = PieceStats { hp :: Int, attack :: Int} deriving (Eq)

data Position = Position {row :: Int, col :: Int} deriving (Eq)

data Square = EmptySquare | OccupiedSquare PieceType Bool PieceStats deriving (Eq)
type GameBoard = [[Square]]

assasinStats :: PieceStats
assasinStats = PieceStats {
  hp = 3,
  attack  = 3
}

skirmisherStats :: PieceStats
skirmisherStats = PieceStats {
  hp = 5,
  attack = 2
}

defenderStats :: PieceStats 
defenderStats = PieceStats{
  hp = 7,
  attack = 1
}

-- | Creates an empty board
emptyBoard :: Int -> Int -> GameBoard
emptyBoard width height = replicate height (replicate width EmptySquare)

-- | Changes the square on a given coordinates and returns a updated board with the new square
setSquare :: GameBoard -> (Int, Int) -> Square -> GameBoard
setSquare board (x, y) square = take y board
                              ++ [take x row ++ [square] ++ drop (x + 1) row]
                              ++ drop (y + 1) board
    where row = board !! y

-- | Takes a board and gives us the square on the given coordinates
getSquare :: GameBoard -> Position -> Square
getSquare board (Position row col) = board !! row !! col

emptySquares :: GameBoard -> [Position]
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
showPiece (Piece pt False stats) = showPieceType pt
showPiece (Piece pt True stats) = toLower (showPieceType pt)


-- | Places a piece on a given coordinate on the board.
placePiece :: GameBoard -> (Int, Int) -> (PieceType, Bool, PieceStats) -> GameBoard
placePiece board (x, y) (pieceType, isEnemy, stats) = setSquare board (x, y) (OccupiedSquare pieceType isEnemy stats)

-- | Removes a piece on a given position and replaces it with an empty square
removePiece :: GameBoard -> (Int, Int) -> GameBoard
removePiece board (x, y) = setSquare board (x, y) EmptySquare

-- | We flatten the board into a single list of squares
-- | Then we filter the list by checking if each square in the list is an Occupiedsquare with a piece represented by an uppercase letter.
-- | We then return the lenght of the filtered list to get how many pieces of the type we want there is.
countPieces :: GameBoard -> PieceType -> Int
countPieces board pieceType = length $ filter isMatch (concat board)
  where
    isMatch (OccupiedSquare pt False stats) = pt == pieceType
    isMatch _ = False


-- | Represents each square depending on if it is empty or occupied, and if the square has an ally or enemy piece.
showSquare :: Square -> Char
showSquare EmptySquare = '.'
showSquare (OccupiedSquare pieceType isEnemy stats) = if isEnemy then toLower (showPieceType pieceType) else showPieceType pieceType

-- | Maps over each square to find their character representation using showSquare. It then adds each character representation to a list
showRow :: [Square] -> String
showRow = intercalate "" . map (return . showSquare)

showBoard :: GameBoard -> String
showBoard = unlines . map showRow

printBoard :: GameBoard -> IO ()
printBoard = putStrLn . showBoard

doAttack :: GameBoard -> Position -> Position -> GameBoard
doAttack board pos1 pos2 =
  let attacker = getSquare board pos1
      attacked = getSquare board pos2
  in case (attacker, attacked) of
        (_, EmptySquare) -> board -- Cannot attack an empty square
        (OccupiedSquare _ _ attStats, OccupiedSquare pt isEnemy defStats)
          | hp defStats > 0 -> let newHp = hp defStats - attack attStats
                                   newSquare = if newHp > 0
                                                 then OccupiedSquare pt isEnemy defStats { hp = newHp }
                                                 else EmptySquare
                               in setSquare board (row pos2, col pos2) newSquare
          | otherwise -> board -- Cannot attack a piece with 0 HP
        _ -> board 


