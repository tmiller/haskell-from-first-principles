module Cipher where

import Data.Char ( ord
                 , chr
                 , isUpper
                 , isAlpha
                 )

indexA :: Char -> Int
indexA c = if isUpper c then ord 'A' else ord 'a' 

index :: Char -> Int
index c = (ord c - indexA c)

encode :: Int -> Char -> Char
encode n c
  | isAlpha c = chr $ (indexA c) + mod (n + index c ) 26
  | otherwise = c

unCaesar :: Int -> String -> String
unCaesar = caesar . negate

caesar :: Int -> String -> String
caesar n = map (encode n)

rot13 :: String -> String
rot13 = caesar 13

vigenere :: String -> String -> String
vigenere = zipWith (\x y -> encode (index y) x)
