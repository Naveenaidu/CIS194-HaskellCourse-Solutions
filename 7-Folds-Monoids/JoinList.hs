{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Sized
import Scrabble
import Buffer
import Editor

import Data.Monoid
import Data.Maybe

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
        deriving (Eq, Show)

-- EXERCISE 1: Operations on JoinList
-- ==================================

-- Thought this might be useful, let's see.
-- instance Monoid JoinList where
--     mempty = Product 1
--     Product x `mappend` Product y =  Product (x * y)


tag :: Monoid m => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag _ = mempty

-- Append two JoinList
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 =  Append (tag jl1 <> tag jl2) jl1 jl2

-------------------------------------------------------------------------------

-- EXERCISE 2: Annotation for fast indexing
-- ========================================

-- > 2.1: A function to find the JoinList elem at index `i` in list

-- Error in compiling file `Sized.hs`
-- Follow this link for explantion - https://stackoverflow.com/questions/53622428/a-basic-monoid-definition-gives-no-instance-for-semigroup-mymonoid-arising-fr

-- To solve t2.1, I had to go through the Finger Trees
-- Link: https://apfelmus.nfshost.com/articles/monoid-fingertree.html

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i (Single _ val)
    | i == 0 = Just val
    | otherwise = Nothing
indexJ i (Append m jleft jright)
    | i < 0 || i > sizeT = Nothing
    | i < sizeL          = indexJ  i jleft
    | otherwise          = indexJ (i - sizeL) jright
        where sizeT = getSize . size $ m 
              sizeL = getSize .size. tag $ jleft
indexJ _ _ = Nothing

-- > 2.2: Drop the first n elements from the JoinList
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n s@(Single _ val)
    | n == 0 = s
dropJ n l@(Append m jLeft jRight)
    | n > sizeT  = Empty
    | n >= sizeL            = dropJ (n - sizeL) jRight
    | (n > 0) && (n < sizeL) = dropJ n jLeft  +++ jRight
    | otherwise  = l
        where sizeT = getSize . size $ m
              sizeL = getSize . size . tag $ jLeft
dropJ _ _  = Empty

-- > 2.3: Take the first n elements from the JoinList
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n s@(Single _ _)
    | n == 1 = s
takeJ n l@(Append m jLeft jRight)
    | n >= sizeT = l
    | n >= sizeL             = jLeft +++ takeJ (n-sizeL) jRight
    | (n > 0) && (n < sizeL) = takeJ n jLeft
        where sizeT = getSize . size $ m
              sizeL = getSize . size . tag $ jLeft
takeJ _ _  = Empty

-- Following code is used for testing. Redundant.
(!!?) :: [a] -> Int -> Maybe a
[]     !!?_     = Nothing
_!!? i | i < 0  = Nothing
(x:xs) !!? 0    = Just x
(x:xs) !!? i    = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty          = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-------------------------------------------------------------------------------

-- EXERCISE 3: Testing Scrabble function
-- =====================================

scoreLine :: String -> JoinList Score String
scoreLine s = (Single (scoreString s) s)

-------------------------------------------------------------------------------

-- EXERCISE 4: Combing anotations
-- =====================================

instance Buffer (JoinList (Score, Size) String) where
    toString             = unlines . jlToList
    fromString           = foldl (\jl str -> jl +++ scoreLine' str) Empty . lines
                           where scoreLine' str = Single (scoreString str, 1) str
    line                 = indexJ
    replaceLine n str jl = takeJ n jl +++ fromString str +++ dropJ (n+1) jl
    numLines             = getSize . snd . tag
    value                =  getScore . fst . tag

-- main = runEditor editor (fromString "test" :: (JoinList (Score, Size) String))

testImplementation :: IO()
testImplementation = runEditor editor (fromString "test" :: (JoinList (Score, Size) String))

-- Testing
a = Append (Size 3)
      (Append (Size 2)
        (Single (Size 1) "hi")
        (Single (Size 1) "bye")
      )
     (Single (Size 1) "tschau")



