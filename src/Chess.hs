module Chess where

import Data.List
import Data.String.ToString

data ChessmanType = Pawn 
                  | Bishop
                  | Knight
                  | Castle
                  | Queen
                  | King
                  | Free
                  | Line

data Chessman = Chessman {
  chessmanType :: ChessmanType,
  symbol       :: String
}

instance Show Chessman where
    show = symbol

instance ToString Chessman where
    toString = symbol

type Board = [[Chessman]]

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

emptyCell = Chessman Free "  "

lineString = replicate 8 $ Chessman Line "--"
butyLines = replicate 7 lineString

--zipLists arr1 arr2 = helper arr1 arr2 [] where
--  helper (x:xs) (y:ys) accum = helper xs ys (accum ++ [y] ++ [x])
--  helper [] (y:[]) accum = accum ++ [y]

initialBoard = [[blackCastle, blackKnight, blackBishop, blackQueen, blackKing, blackBishop, blackKnight, blackCastle],
    replicate 8 blackPawn,
    replicate 8 emptyCell,
    replicate 8 emptyCell,
    replicate 8 whitePawn,
    [whiteCastle, whiteKnight, whiteBishop, whiteQueen, whiteKing, whiteBishop, whiteKnight, whiteCastle]]



printBoard :: Board -> IO ()
printBoard xxs = mapM_ printLine butyBoard
    where printLine xs = putStrLn ("|" ++ (intercalate "|" (map toString xs)) ++ "|")
          zipLists arr1 arr2 = helper arr1 arr2 []
              where
                  helper (x:xs) (y:ys) accum = helper xs ys (accum ++ [y] ++ [x])
                  helper [] (y:[]) accum = accum ++ [y]
          butyBoard = zipLists xxs butyLines

