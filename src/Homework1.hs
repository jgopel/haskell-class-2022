module Homework1 where

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0    = []
  | otherwise = x `mod` 10 : toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x * 2]
doubleEveryOther (x:(y:xs)) = x*2 : y : doubleEveryOther xs

sumDigitsImpl :: [Integer] -> [Integer]
sumDigitsImpl [] = []
sumDigitsImpl (x:xs) = toDigits x ++ sumDigitsImpl xs

sumDigits :: [Integer] -> Integer
sumDigits = sum . sumDigitsImpl

validate :: Integer -> Bool
validate x = (sumDigits(doubleEveryOther(toDigits(x))) `mod` 10) == 0
