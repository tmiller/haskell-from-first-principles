module Sample where

import Data.Char (toUpper, toLower)
import Data.Bool (bool)
import Data.List (intercalate)

eft :: (Enum a, Ord a) => a -> a -> [a]
eft a b
  | a == b    = [b]
  | a < b     = a : eft (succ a) b
  | otherwise = []

myWords :: String -> [String]
myWords []         = []
myWords (' ' : xs) = myWords xs
myWords phrase     = takeWhile (/= ' ') phrase
                   : myWords (dropWhile (/= ' ') phrase)


itIsMystery :: [Char] -> [Bool]
itIsMystery xs =
  map (\x -> elem x "aeiou") xs
  
myZip :: [a] -> [b] -> [(a,b)]
myZip = myZipWith (,)

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
myZipWith _ _ _ = []

cap :: String -> String
cap (x:xs) = toUpper x : cap xs
cap _ = []

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = e == x || myElem e xs

myReverse :: [a] -> [a]
myReverse = myReverse' []
  where myReverse' result []     = result
        myReverse' result (x:xs) = myReverse' (x : result) xs

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

minMax :: Ordering -> (a -> a -> Ordering) -> [a]-> a
minMax _ _ []     = undefined
minMax o f (x:xs) = minMax' x xs
  where 
    minMax' r []     = r
    minMax' r (y:ys) = minMax' (comp r y) ys
    comp r y         = bool y r (f r y == o)

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = minMax GT

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = minMax LT

myMaximum :: Ord a => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: Ord a => [a] -> a
myMinimum = myMinimumBy compare

notThe :: String -> Maybe String
notThe xs = case map toLower xs /= "the" of
  True  -> Just xs
  False -> Nothing

replaceThe :: String -> String
replaceThe phrase = intercalate " " $ replaceThe' $ words phrase
  where
    replaceThe' []     = []
    replaceThe' (x:xs) =
      case notThe x of
        Nothing     -> "a" : replaceThe' xs
        (Just word) -> word : replaceThe' xs

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel  = countTheBeforeVowel' . words 
  where
    countTheBeforeVowel' (x:y:xs) = 
      case (notThe x, elem (head y) "aeiouy") of
        (Nothing, True) -> 1 + countTheBeforeVowel' (y:xs)
        _               -> countTheBeforeVowel' (y:xs)
    countTheBeforeVowel' _ = 0
  
myIterate :: (a -> a) -> a -> [a]
myIterate f = myUnfoldr (\b -> Just (b, f b))

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f acc =  go acc
  where go b = case f b of
               Just (a, new_b) -> a : go new_b
               Nothing         -> []
