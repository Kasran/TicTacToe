module Main where

import TicTacToe
import Data.List (intercalate)
import Data.Char (toLower)
import GHC.IO.Handle (hFlush)
import GHC.IO.Handle.FD (stdout)
import Control.Monad (when, guard)

readSafe :: Read a => String -> Either String a
readSafe s = case (reads s :: Read a => [(a, String)]) of
    [(x,"")] -> Right x
    _ -> Left s

readMaybe :: Read a => String -> Maybe a
readMaybe = either (const Nothing) Just . readSafe

prompt :: String -> IO String
prompt str = do
    putStr str
    hFlush stdout
    getLine

getYesNo :: String -> IO Bool
getYesNo str = do
    r <- prompt str
    case (map toLower r) of
        'y':_ -> return True
        'n':_ -> return False
        _     -> getYesNo "(please enter y/n) "

readValidMove :: Board -> String -> Maybe Int
readValidMove brd s = do
    mv <- readMaybe s
    mv <$ guard (checkMove brd mv)

getMove :: Board -> String -> IO Int
getMove brd str = do
    putStrLn $ "Valid moves are "++(intercalate ", " . map show $ validMoves brd)
    mvs <- prompt str
    case (readValidMove brd mvs) of
        Nothing -> getMove brd str
        Just x -> return x

gameLoop :: Board -> Piece -> IO (Board, Maybe Piece)
gameLoop brd p = do
    putStrLn ""
    putStrLn $ showBoard brd
    move <- getMove brd $ "It's "++(show p)++"'s turn: "
    let (_, brd') = putPiece p move brd
    case (checkMatch brd') of
        Just winner -> return (brd', Just winner)
        Nothing ->
            if (null $ validMoves brd') then return (brd', Nothing)
            else gameLoop brd' $ opponent p
--

main :: IO ()
main = putStrLn "\nWelcome to Tic Tac Toe!" >> go where
    go = do
        (brd, winner) <- gameLoop (replicate 9 Nothing) X
        putStrLn ""
        putStrLn $ showBoard brd
        putStrLn $ "The winner is "++(maybe "nobody" show winner)++"!"
        yn <- getYesNo "Play again? (y/n) "
        when yn go
--





