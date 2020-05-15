-- Problem 1: Validity check for Credit Cards

-- Excercise1 : Find the digits of the number
-- toDigits    :: Integer -> [Integer]
-- toDigitsRev :: Integer -> [Integer]

-- Example: toDigits 1234 == [1, 2, 3, 4]
-- Example: toDigitRev 1234 == [4, 3, 2, 1]
-- Example: toDigits 0 == []
-- Example: toDigits (-17) == []

-- toDigits :: Integer -> [Integer]
-- toDigits

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x = 
  if x < 0
    then []
    else toDigits (x `div` 10) ++ [x `mod` 10]
  

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev x = 
  if x <0
    then []
    else (x `mod` 10) : toDigitsRev (x `div` 10)

---- EXERCISE 2: Double every other integer in list
-- Example:doubleEveryOther [8,7,6,5] == [16,7,12,5]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther xs =
  if even (length xs)
    then head xs * 2 : doubleEveryOther (tail xs)
    else head xs     : doubleEveryOther (tail xs)

---- EXERCISE 3: Sum of all digits in the list
-- sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits xs = sum (toDigits (head xs) ++ [sumDigits (tail xs)])

---- EXERCISE 4: Validate the credit card
-- Eg: validate 4012888888881881 = True
validate :: Integer -> Bool
validate x = 
  if (sumDigits (doubleEveryOther (toDigits x ))) `mod` 10 == 0
    then True
    else False