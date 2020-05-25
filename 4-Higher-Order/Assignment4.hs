import Data.List

-- EXERCISE 1: Wholemeal programming
-- =================================
-- > Convert the  functions into a wholemeal programming style
-- fun1 :: [Integer] -> Integer
-- fun1 []     = 1
-- fun1 (x:xs)
--     | even x    = (x - 2)*fun1 xs
--     | otherwise = fun1 xs 

fun1' :: [Integer] -> Integer
fun1' xs = product $ map (subtract 2) (filter even xs)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3*n + 1)

fun2' :: Integer -> Integer
fun2' =  sum
        . filter even
        . takeWhile (/=1)
        . iterate (\x -> if even x then x`div` 2 else 3*x+1)

-- EXERCISE 2: Build a balanced Binary Tree using foldr
-- =====================================================

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

heightTree :: Tree a -> Integer
heightTree Leaf = -1
heightTree (Node n _ _ _) = n

insertTree :: a -> Tree a -> Tree a
insertTree elem Leaf = Node 0 Leaf elem Leaf
insertTree elem (Node n ltree val rtree)
    | hrtree > hltree = Node n (insertTree elem ltree) val rtree
    | hrtree < hltree = Node n ltree val (insertTree elem rtree)
    | otherwise       = Node (hrtree + 2) ltree val (insertTree elem rtree)
   where hrtree = heightTree rtree
         hltree = heightTree ltree

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf 

-- EXERCISE 3: More folds!
-- =======================

-- > 1. Implement a function which returns True for odd number of True
-- > xor [False, True, False] == True
-- > xor [True, True] == False

xor1 :: Bool -> Bool -> Bool
xor1 True False = True
xor1 False True = True
xor1 _ _ = False

xor :: [Bool] -> Bool
xor = foldl xor1 False 

-- > 2. Implement map as a fold.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- > 3. Implement foldl using foldr
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x acc -> f acc x) base (reverse xs)

-- A better verion for flip suggested by hlint
-- myFold2 :: (a -> b -> a) -> a -> [b] -> a
-- myFold2 f base xs = foldr (flip f) base $ reverse xs

-- EXERCISE 4: Finding Primes
-- ==========================

-- > Cartesian Product
cartProd :: [a] -> [b]  -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-- > Sieve of Sundaram
--- > Get the (i,j) of all the elements that satisfies the condition 
--- > i + j + 2*i*j > nNew. Then using these i,j make the list number that is 
--- > described in the format of i + j + 2*i*j. Let's assume this list as
--- > `nonPrime`.
--- > Now from the list of [3..n], remove the numbers that are present in the
--- > the `nonPrime list.
--- > NOTE: sieveSundaram's algo does not create 2 as the prime number in list,
--- >       hence add 2 to the start of the list.
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2 : ( map (\x -> 2 * x + 1) $ [1..nNew] \\ nonPrime )
            where 
                  nonPrime = map (\(i,j) -> i + j + 2*i*j)
                             . filter (\(i,j) -> i + j + 2*i*j <= nNew) $ xs
                  nNew = (n-2) `div` 2
                  xs = cartProd [1,2..nNew] [1,2..nNew]