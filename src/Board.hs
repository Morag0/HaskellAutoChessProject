module Board (
    assasinStats,
    skirmisherStats,
    defenderStats,
    emptyBoard,
    setSquare,
    getSquare,
    emptySquares,
    displayPositions,
    showSquare,
    removePiece,
    countPieces,
    showRow,
    showBoardWithPieces,
    attackPiece,
    initBoard,
    placePiece,
    movePieceOnBoard,
    GameBoard,
    Square(..),
    Piece(..),
    PieceType(..),
    PieceStats(..),
    BoardPosition(..)
) where

import Data.List (intercalate)
import Data.Char(toLower)

-- | There are three types of pieces, Assassins, Defenders and Skirmishers
data PieceType = Assassin | Skirmisher | Defender deriving (Eq, Show)
-- | A piece has a pieceType(Either Assassin, Skirmisher or Defender), a boolean that decides if the piece is the players piece, and stats depending on what type of piece it is
data Piece = Piece { pieceType :: PieceType, isEnemy :: Bool, pieceStats :: PieceStats } deriving (Eq)
-- | For Simplicity a piece has only hp and attack stats
data PieceStats = PieceStats { hp :: Int, attack :: Int} deriving (Eq, Show)

-- | A position on the board is made up of a tuple of two ints
data BoardPosition = Position {row :: Int, col :: Int} deriving (Eq, Ord, Show, Read)
-- | A square on the board can either be empty or occupied. If it is occupied we also would like to know the specifics of the piece that occupies it
data Square = EmptySquare | OccupiedSquare PieceType Bool PieceStats deriving (Eq, Show)
-- | The game board itself is made up of a a nested list of squares
type GameBoard = [[Square]]

-- | Here we define the stats of the different pieces, some have more hp, while others have more attack
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

-- | emptyBoard takes a width and a height as integers and creates an emptyboard
emptyBoard :: Int -> Int -> GameBoard
emptyBoard width height = replicate height (replicate width EmptySquare)

-- | SetSquare takes a board and a position on it aswell as the square to be set on that position, and changes the board, giving us a new one with the specified square replaced.Assassin
-- | It does this by first splitting the board in two, top and bottom. The top is the top y rows while the bottom is the rest. It then splits the bottom into two, left and right based on the x column
-- | It then concatenates the different pieces and we get a modified board with the same dimensions
setSquare :: GameBoard -> (Int, Int) -> Square -> GameBoard
setSquare board (x, y) square =
    let (top, bottom) = splitAt y board
        (left, right) = splitAt x (head bottom)
    in top ++ [left ++ square : tail right] ++ tail bottom

-- | getSquare gives us the square on a given position on the board
-- | It does so by taking the first rows+1 rows and columns+1 squares from each of these rows. It then reverses the list and taakes the head element which would be the specified square
getSquare :: GameBoard -> BoardPosition -> Square
getSquare board (Position row col) =
    let rows = take (row + 1) board
        squares = map (take (col + 1)) rows
    in case reverse (head squares) of
         square : _ -> square
         []         -> error "Empty row in game board"

-- | emptySquares lets us find all empty squares on the board
-- | it does this by using list comprehension to iterate over all the positions on the board and checks if it is empty by using getSquare to determine tha status of the squaree (EmptySquare or OccupiedSquare)
emptySquares :: GameBoard -> [BoardPosition]
emptySquares board = [Position x y | x <- [0..width-1], y <- [0..height-1], getSquare board (Position x y) == EmptySquare && y >= height `div` 2]
  where
    width = length (head board)
    height = length board

-- | displayPositions takes a list of positions on the board and displays them, seperating them by comma.Assassin
-- | intercalate from Data.List is used to join the positions into a single string , while show is used to convert each positions into its string representation
displayPositions :: [BoardPosition] -> String
displayPositions positions = intercalate ", " (map show positions)


-- | Places a piece on a given coordinate on the board.
-- | It uses setSquare to set the square at a specified position to an occupiedSquare with the specified pieceType etc
placePiece :: GameBoard -> (Int, Int) -> (PieceType, Bool, PieceStats) -> GameBoard
placePiece board (x, y) (pieceType, isEnemy, stats) = setSquare board (x, y) (OccupiedSquare pieceType isEnemy stats)

-- | Removes a piece on a given position and replaces it with an empty square
-- | similar to place piece, but here we replace an occupiedSquare with an empty Square
removePiece :: GameBoard -> (Int, Int) -> GameBoard
removePiece board (x, y) = setSquare board (x, y) EmptySquare

-- | Counts all the pieces of a given pieceType
-- | It does this by flatttening the board to a list of squares using concat, then filters out the occpoiedSquares with a matching pieceType and counts them by finding the length of the filtered list
countPieces :: GameBoard -> PieceType -> Int
countPieces board pieceType = length $ filter isMatch (concat board)
  where
    isMatch (OccupiedSquare pt False stats) = pt == pieceType
    isMatch _ = False


-- | Allows us to differentiate between emptySquares and different pieceTypes
showSquare :: Square -> String
showSquare EmptySquare = "."
showSquare (OccupiedSquare pieceType isEnemy stats) = case pieceType of
      Assassin -> if not isEnemy then "A" else "a"
      Skirmisher -> if not isEnemy then "S" else "s"
      Defender -> if not isEnemy then "D" else "d"

-- | Takes a list of squares which represents a row on the board, and returns the string representation of that row
-- | Each square is revealed by showSquare and concatonated togheter with a space between them.
showRow :: [Square] -> String
showRow row = concatMap showSquare row ++ " "

-- | Takes a board and returns the string representation
showBoardWithPieces :: GameBoard -> String
showBoardWithPieces board = intercalate "\n" $ map showRow board

-- | Creates a board by using a combination of emptyBoard and placePiece. This is used as the initial board in the game
initBoard :: GameBoard
initBoard = let board = emptyBoard 6 6
                topRow = [(x, 0) | x <- [0..5]]
                boardWithSkirmisher = placePiece board (head topRow) (Skirmisher, True, skirmisherStats)
                boardWithDefender = placePiece boardWithSkirmisher (1, 0) (Defender, False, defenderStats)
            in boardWithDefender



attackPiece :: GameBoard -> BoardPosition -> BoardPosition -> GameBoard
attackPiece board pos1 pos2 =
  let square1 = getSquare board pos1
      square2 = getSquare board pos2
      board1 = if square2 == EmptySquare
                 then board
                 else setSquare board (row pos2, col pos2) EmptySquare
      board2 = setSquare board1 (row pos1,col pos1) EmptySquare
  in case square1 of
       OccupiedSquare pieceType1 isEnemy1 pieceStats1 ->
         case square2 of
           OccupiedSquare pieceType2 isEnemy2 pieceStats2 ->
             if isEnemy1 /= isEnemy2
               then setSquare board2 (row pos2, col pos2) (OccupiedSquare pieceType1 isEnemy2 pieceStats2)
               else board2
           EmptySquare -> setSquare board2 (row pos2, col pos2) (OccupiedSquare pieceType1 False pieceStats1)
       EmptySquare -> board2

-- | This function moves a piece from one position on the game board to another position. The input parameters are the game board, the initial position of the piece, and the destination position of the piece. 
-- | It first checks whether the destination square is empty or not. 
-- | If the destination square is empty, it moves the piece from the initial position to the destination position by setting the square at the initial position to empty and the square at the destination position to the piece. 
-- |Otherwise, it leaves the board unchanged. Finally, it returns the updated game board.
movePieceOnBoard :: GameBoard -> BoardPosition -> BoardPosition -> GameBoard
movePieceOnBoard board pos1 pos2 =
  let square1 = getSquare board pos1
      square2 = getSquare board pos2
      board1 = if square2 == EmptySquare
                 then setSquare board (row pos1,col pos1) EmptySquare
                 else board
      board2 = setSquare board1 (row pos2,col pos2) square1
  in board2


