import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
              (fromGregorian 1911 5 1)
              (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
              (fromGregorian 1921 5 1)
              (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs = [ time | DbDate time <- xs ]

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs =  [ n | DbNumber n <- xs ]

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = let zero = (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0))
             in  foldr max zero . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = let items     = fromIntegral $ length $ filterDbNumber xs
               summation = fromIntegral $ sumDb xs
           in summation / items
