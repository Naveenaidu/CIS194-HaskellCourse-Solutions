module Party where

import Data.Tree
import Data.List

import Employee

-- EXERCISE 1: Tools to work witj GuestLists
-- =========================================

-- Add an employee to the GuestList
glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp {empName = name,empFun = fun}) (GL list totalFun) =
    GL  (emp:list)  (fun + totalFun)

-- A Monoid Instance for GuestList
instance Monoid GuestList where
    mempty = GL [] 0

instance Semigroup GuestList where
    (GL list1 fun1) <> (GL list2 fun2) = GL (list1 ++ list2) (fun1 + fun2)

-- Forgot that we had made ORD instance of GuestList
-- Return GuestList with more fun
-- moreFun :: GuestList -> GuestList -> GuestList
-- moreFun g1@(GL _ f1) g2@(GL _ f2)
--     | f1 > f2   = g1
--     | f2 > f1   = g2
--     | otherwise = g1

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1 g2 = if g1 > g2 then g1 else g2

-------------------------------------------------------------------------------

-- EXERCISE 2: Fold function for Tree
-- ===================================

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f initializer (Node {rootLabel = rl, subForest = sf}) = 
    f rl (map (treeFold f initializer) sf)

-------------------------------------------------------------------------------

-- EXERCISE 3: Find the next level of guest list
-- =============================================

-- Return : (maxFunGuestListWithEmp, maxFunGuestListWithoutEmp)
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp gListTuple   = (maxFunWithEmp, maxFunWithoutEmp)
    where flatgListTuple   = concat [[a,b] | (a,b) <- gListTuple]
          maxFunWithEmp    = maximumGList (map (glCons emp) flatgListTuple)
          maxFunWithoutEmp = maximumGList flatgListTuple

maximumGList :: (Monoid a, Ord a) => [a] -> a
maximumGList [] = mempty
maximumGList xs = maximum xs
-------------------------------------------------------------------------------

-- EXERCISE 4: Fun Maximixing GuestList from the Employee Tree
-- ===========================================================
maxFun :: Tree Employee -> GuestList
maxFun tree = uncurry moreFun $ treeFold nextLevel (mempty, mempty) tree

-------------------------------------------------------------------------------

-- EXERCISE 5: Implement the main IO block
-- ========================================

main = do readFile "company.txt"  >>= putStrLn . computeOutput

computeOutput :: String -> String
computeOutput employee = formatGL $ maxFun $ read employee

formatGL :: GuestList -> String
formatGL (GL lst fun) = "Total fun: " ++ show fun ++ "\n" ++ unlines guestNames
    where guestNames = sort $ map (\Emp {empName = name} -> name) lst