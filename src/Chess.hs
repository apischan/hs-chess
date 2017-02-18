module Chess where

import Data.List
import Data.String.ToString
import Data.Sequence hiding (replicate)

data ChessmanType = Pawn 
                  | Bishop
                  | Knight
                  | Castle
                  | Queen
                  | King

data Placeholder = Free
                 | Line

data CellPlaceholder = SpecialPlaceholder { placeholder :: Placeholder,
                                            symbol      :: String}
                     | Chessman { chessmanType :: ChessmanType,
                                  symbol       :: String }

instance ToString CellPlaceholder where
    toString = symbol

type Board = [[CellPlaceholder]]

blackPawn = Chessman Pawn "♟ "
whitePawn = Chessman Pawn "♙ "

blackKnight = Chessman Knight "♞ "
whiteKnight = Chessman Knight "♘ "

blackBishop = Chessman Bishop "♝ "
whiteBishop = Chessman Bishop "♗ "

blackCastle = Chessman Castle "♜ "
whiteCastle = Chessman Castle "♖ "

blackQueen = Chessman Queen "♛ "
whiteQueen = Chessman Queen "♕ "

blackKing = Chessman King "♚ "
whiteKing = Chessman King "♔ "

emptyCell = SpecialPlaceholder Free "  "
linePlaceholder = SpecialPlaceholder Line "--"

lineString = replicate 8 $ linePlaceholder
butyLines = replicate 7 lineString

initialBoard = [[blackCastle, blackKnight, blackBishop, blackQueen, blackKing, blackBishop, blackKnight, blackCastle],
    replicate 8 blackPawn,
    replicate 8 emptyCell,
    replicate 8 emptyCell,
    replicate 8 whitePawn,
    [whiteCastle, whiteKnight, whiteBishop, whiteQueen, whiteKing, whiteBishop, whiteKnight, whiteCastle]]

printBoard :: Board -> IO ()
printBoard xxs = mapM_ printLine butyBoard
    where printLine xs = putStrLn ("|" ++ (intercalate "|" (map toString xs)) ++ "|")
          zipLists board placeholder = helper board placeholder []
              where
                  helper (x:xs) (y:ys) accum = helper xs ys (accum ++ [y] ++ [x])
                  helper [] (y:[]) accum = accum ++ [y]
          butyBoard = zipLists xxs butyLines

