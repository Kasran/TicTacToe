module TicTacToe
    ( Piece(X,O),
      opponent,
      Board,
      matches,
      checkMatch,
      validMoves,
      checkMove,
      putPiece,
      showBoard
    ) where

import Data.List (uncons, unfoldr, intercalate)
import Data.Maybe (mapMaybe, listToMaybe, isNothing)
import Control.Monad (when, guard)

(-->) :: a -> (a -> b) -> b
(-->) = flip ($)

foreach :: [a] -> (a -> b) -> [b]
foreach = flip map

replace :: Int -> a -> [a] -> [a]
replace i x xs = let (ys, zs) = splitAt i xs in ys++(x:(tail zs))

collapse :: Eq a => [Maybe a] -> Maybe a
collapse (x:xs) | all (== x) xs = x
collapse _ = Nothing


collapse' :: Eq a => [Maybe a] -> Maybe a
collapse' (x:xs) = foldr (\x y -> if (x == y) then x else Nothing) x xs
collapse' [] = Nothing

safeHead :: a -> [a] -> a
safeHead d = maybe d fst . uncons

groups :: Int -> [a] -> [[a]]
groups n
  | n > 0 = unfoldr (\b -> splitAt n b <$ guard (not $ null b))
  | otherwise = (:[])



--------- actual tic tac toe stuff

data Piece = X | O deriving (Show, Eq)

opponent :: Piece -> Piece
opponent X = O
opponent O = X

type Board = [Maybe Piece]

matches :: [[Int]]
matches = [[0,1,2],[3,4,5],[6,7,8],
           [0,3,6],[1,4,7],[2,5,8],
           [0,4,8],[2,4,6]]

checkMatch :: Board -> Maybe Piece
checkMatch brd = (foreach matches $ map (brd !!)) --> mapMaybe collapse --> listToMaybe

validMoves :: Board -> [Int]
validMoves brd = map snd $ filter (isNothing . fst) $ zip brd [0..]

checkMove :: Board -> Int -> Bool
checkMove brd pos = elem pos $ validMoves brd

putPiece :: Piece -> Int -> Board -> (Maybe Piece, Board)
putPiece x pos brd = case (brd !! pos) of
    p@(Just _) -> (p, brd)
    Nothing    -> (Nothing, replace pos (Just x) brd)

showBoard :: Board -> String
showBoard brd = groups 3 brd --> map (intercalate " " . map sp) --> intercalate "\n"
    where sp = maybe "." show