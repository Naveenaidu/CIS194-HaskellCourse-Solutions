module Fibonacci where

import Data.List
-- EXERCISE 1: Fibonacci Numbers
-- ==================================
fib :: Integer -> Integer
fib n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = fib (n-1) + fib (n-2)

fib1 :: [Integer]
fib1 = map fib [0..]
-------------------------------------------------------------------------------

-- EXERCISE 2: Optimized Fibonnaci function
-- =========================================

-- Why did foldl not work??
-- fib2 :: [Integer]
-- fib2 = foldl' (\acc x -> acc ++ [acc!! (x-1) + acc !! (x-2)] ) [0,1] [2..]

fib2 :: Integer -> Integer -> [Integer]
fib2 a b = [a+b] ++  fib2 b (a+b)

fibs2 :: [Integer]
fibs2 = 0 : 1 : fib2 0 1

-------------------------------------------------------------------------------

-- EXERCISE 3: Streams 
-- ====================

data Stream a = Cons a (Stream a)

-- Convert Stream to infinite list
streamToList :: Stream a -> [a]
streamToList (Cons elem stream) = [elem] ++ streamToList stream 

instance Show a => Show (Stream a) where
    show stream = show . take 20 $ streamToList  stream

-------------------------------------------------------------------------------

-- EXERCISE 4: Tools for Stream 
-- ============================

-- Generate a stream containing infinitely many copies of thegiven element
streamRepeat :: a -> Stream a
streamRepeat elem = Cons elem (streamRepeat elem)

-- Apply a function to every element of a Stream
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons elem stream) = Cons (f elem) (streamMap f stream)

-- Stream from a seed
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))