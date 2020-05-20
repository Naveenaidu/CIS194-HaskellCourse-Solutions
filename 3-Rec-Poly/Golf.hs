module Golf where

import Data.List
import Data.Maybe

-- Skip n Elements
-- > skipNElem [1,2,3,4,5,6] 2 == [2,4,6]
-- > skipNElem "hello!" 2      == "el!"
-- > Jumps every n Elements until the end of list
skipNElem :: Eq xs => [xs]-> Int -> [xs]
skipNElem xs jmpIndex = 
    let jmpIndices = [jmpIndex -1, jmpIndex-1+jmpIndex .. length xs -1] in
    [xs !! jmpIndex |jmpIndex <- jmpIndices]

-- EXERCISE 1: Hopscotch
-- ===================== 
-- > skips "ABCD"       == ["ABCD", "BD", "C", "D"]
-- > skips "hello!"     == ["hello!", "el!", "l!", "l", "o", "!"]
-- > skips [1]          == [[1]]
-- > skips [True,False] == [[True,False], [False]]
-- > skips []           == []
skips :: Eq xs => [xs] -> [[xs]]
skips xs = [skipNElem xs (n+1) | n <- [0,1..length xs - 1]]

-- EXERCISE 2: Local Maxima
-- =====================
-- > No local Maxima when one or two elements
-- > Calculate the local maxima between the second element to second last
-- > localMaxima [2,9,5,6,1] == [9,6]
-- > localMaxima [2,3,4,1,5] == [4]
-- > localMaxima [1,2,3,4,5] == []
localMaxima :: [Integer] -> [Integer]
localMaxima xs = [xs!!n | n <- [1,2..length xs -2], 
                          xs!!n > xs!!(n-1), xs!!n > xs!!(n+1)]

-- EXERCISE 3: Histogram
-- =====================

-- Count of the number `n` in list `xs`
-- > count [1,1,1,5] 1           == 3
-- > count [1,2,3,4,4,5,6,4,4] 4 == 4  
count :: [Integer] -> Integer -> Integer
count xs num = sum [ 1 | x <- xs, x == num ]

-- Extract the count of each number
-- Returns the count of the number according to the index
-- i.e Index 0 will have the number of 0's in list and so on
-- > extract [1,1,1,5]               == [0,3,0,0,1,0,0,0,0,0]
-- > extarct [1,4,5,4,6,6,3,4,2,4,9] == [0,1,1,1,4,1,2,0,0,1]
extract :: [Integer] -> [Integer]
extract xs = [count xs num | num <- [0,1..9]]

-- Create a string with * at particular indices
-- > createString [4,7,9]       == "    *  * *"
-- > createString [0,1,4,7,9]   == "**  *  * *"
-- > createString [0,1,4,7,8,9] == "**  *  ***"
-- > Which is similar to:
-- > **  *  ***
-- > 0123456789
createString :: [Integer] -> String
createString [] = ""
createString (x:rest) =
    (replicate (fromIntegral x) ' ' ++ "*") ++ suffixString (x:rest)

suffixString :: [Integer] -> String
suffixString (x:[])     = "\n"
suffixString (x:y:rest) = 
    replicate (fromIntegral (y-x-1)) ' ' ++ "*" ++ suffixString(y:rest)

-- Indices of the elems that has the maximum value
getElemIndices :: [Integer] -> Integer -> [Integer]
getElemIndices [] _   = []
getElemIndices xs num = 
    [fromIntegral n | n <- [0,1..length xs-1], xs !!n == num]

-- Update Numbers count
-- After a single run on the list, reduce the elem with max value in list by 1
updateNumberCount :: [Integer] -> [Integer] -> [Integer]
updateNumberCount xs maxElemIndices =
    [if (fromIntegral n) `elem` maxElemIndices then (xs!!n) -1 else xs!!n 
                                            | n <- [0,1..length xs -1]]
                                              
-- Create the Histogram
-- Build the Histogram horizontally, starting from the top
-- Get the indexes which has the maximum value. Draw a `*` at those indices
-- Then move down one layer, that means draw a start at those indices where
-- the the values is maximum value - 1
type NumbersCount = [Integer]

createHistogram :: NumbersCount -> Integer -> String
createHistogram xs 0 =
    let equalString = (replicate 10 '=') ++ "\n"
        indexString = "0123456789\n" in
        equalString ++ indexString
createHistogram xs maxElem = 
    let maxElemIndices = getElemIndices xs maxElem 
        updatedXS = updateNumberCount xs maxElemIndices in
    createString maxElemIndices ++ createHistogram updatedXS (maxElem - 1)

-- Histogram
histogram :: [Integer] -> String
histogram [] = ""
histogram xs = 
    let numbersCount = extract xs
        maxElem      = maximum xs in
        createHistogram numbersCount maxElem