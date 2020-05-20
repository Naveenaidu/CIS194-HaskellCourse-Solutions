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

-- EXERCISE 3: Histogram [BETTER SOLUTION - ORIGINAL SOL IN PREV COMMIT]
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

-- Draw a single line
line :: [Integer] -> Integer -> String
line xs n = [ if x  >= n then '*' else ' '| x <- xs]

-- Histogram
histogram :: [Integer] -> String
histogram xs = unlines (map (line numberCount) [m,m-1..1]) ++ equalString ++ indexString
    where numberCount = extract xs
          m = maximum numberCount
          equalString = replicate 10 '=' ++ "\n"
          indexString = "0123456789\n"
          
