module Homework1 where

toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0    = []
  | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOtherFromFirst :: [Integer] -> [Integer]
doubleEveryOtherFromFirst [] = []
doubleEveryOtherFromFirst (x:[]) = [x]
doubleEveryOtherFromFirst (x:y:xs) = x : y*2 : doubleEveryOtherFromFirst xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherFromFirst . reverse

sumDigitsImpl :: [Integer] -> [Integer]
sumDigitsImpl [] = []
sumDigitsImpl (x:xs) = toDigits x ++ sumDigitsImpl xs

sumDigits :: [Integer] -> Integer
sumDigits = sum . sumDigitsImpl

validate :: Integer -> Bool
validate x = (sumDigits(doubleEveryOther(toDigits(x))) `mod` 10) == 0

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n start end temp = hanoi (n-1) start temp end ++ [(start, end)] ++ hanoi (n-1) temp end start
