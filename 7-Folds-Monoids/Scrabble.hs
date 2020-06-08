{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Char
import Data.Monoid

-- EXERCISE 3: Scrabble score
-- ==========================

newtype Score = Score Int deriving (Eq, Show, Ord, Num)

instance Semigroup Score where
    Score x <> Score y =  Score (x+y)

instance Monoid Score where
    mempty = Score 0

score :: Char -> Score
score l
    | l' `elem` "aeilnorstu" = Score 1
    | l' `elem` "dg"         = Score 2
    | l' `elem` "bcmp"       = Score 3
    | l' `elem` "fhvwy"      = Score 4
    | l' `elem` "k"          = Score 5
    | l' `elem` "jx"         = Score 8
    | l' `elem` "qz"         = Score 10
    | otherwise              = Score 0
        where l' = toLower l

scoreString :: String -> Score
scoreString s = foldl (\acc c -> (score c) <> acc)  (Score 0) s

getScore :: Score -> Int
getScore (Score x) = x