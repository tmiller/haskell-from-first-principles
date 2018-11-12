module Main where

import Text.Read (readMaybe)
import Control.Monad (forever)
import System.Exit (exitSuccess)

import Data.Char ( ord
                 , chr
                 , isUpper
                 , isAlpha
                 )

indexA :: Char -> Int
indexA c = if isUpper c then ord 'A' else ord 'a' 

index :: Char -> Int
index c = ord c - indexA c

encode :: Int -> Char -> Char
encode n c
  | isAlpha c = chr $ indexA c + mod (n + index c ) 26
  | otherwise = c

unCaesar :: Int -> String -> String
unCaesar = caesar . negate

caesar :: Int -> String -> String
caesar n = map (encode n)

rot13 :: String -> String
rot13 = caesar 13

vigenere :: String -> String -> String
vigenere = zipWith (\x y -> encode (index y) x)

printMenu :: IO ()
printMenu = do
  putStrLn "Choose an encryption method"
  putStrLn "  1. Caesar"
  putStrLn "  2. Vigenere"
  putStrLn "  3. Exit"
  putStr   "Choice: "

choose :: IO ()
choose = do
  choice <- (readMaybe <$> getLine ) :: IO (Maybe Integer)
  case choice of
    (Just 1) -> undefined
    (Just 2) -> undefined
    (Just 3) -> exitSuccess
    _        -> putStrLn "\nInvalid choice, please try again."
    

run :: IO ()
run = forever $ do
  printMenu
  choose
  

main :: IO ()
main = run
