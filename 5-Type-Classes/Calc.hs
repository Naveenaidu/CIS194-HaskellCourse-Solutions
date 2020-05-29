{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-} 
module Calc where

import ExprT
import Parser
import StackVM

import Data.Maybe
import VarExprT
import qualified Data.Map as M

-- EXERCISE 1: An Evaluator for ExprT
-- ==================================
-- eval (Mul (Lit 2) (Lit 3)) == 6
-- eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20
eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add exp1 exp2) = eval exp1 + eval exp2
eval (ExprT.Mul exp1 exp2) = eval exp1 * eval exp2

--------------------------------------------------------------------------------

-- EXERCISE 2: Evaluate a String 
-- ==================================
evalStr :: String -> Maybe Integer
evalStr str =
    case parsedExp of
        Just a -> Just (eval a)
        Nothing -> Nothing
    where parsedExp = parseExp ExprT.Lit ExprT.Add ExprT.Mul str

--------------------------------------------------------------------------------

-- EXERCISE 3: Expr Typeclass
-- ==================================
class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a


instance Expr ExprT where
    lit  = ExprT.Lit 
    mul  = ExprT.Mul 
    add  = ExprT.Add 

reify :: ExprT -> ExprT
reify = id

--------------------------------------------------------------------------------

-- EXERCISE 4: Instances of Expr for different types
-- =================================================

instance Expr Integer where
    lit = id
    mul exp1 exp2 = exp1 * exp2
    add exp1 exp2 = exp1 + exp2

instance Expr Bool where
    lit num
        | num <= 0  = False
        | otherwise = True
    mul exp1 exp2 = exp1 && exp2
    add exp1 exp2 = exp1 || exp2 

newtype MinMax  = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
    lit = MinMax
    mul (MinMax x) (MinMax y) = lit $ min x y
    add (MinMax x) (MinMax y) = lit $ max x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
     lit num = Mod7 $ num `mod` 7
     mul (Mod7 x) (Mod7 y) = lit $ x*y
     add (Mod7 x) (Mod7 y) = lit $ x+y

-------------------------------------------------------------------------------
-- Exercise 5
instance Expr StackVM.Program where
  lit i = [StackVM.PushI i]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

-------------------------------------------------------------------------------
-- Exercise 6

class HasVars a where
    var :: String -> a

instance Expr VarExprT where
    lit = VarExprT.Lit
    add = VarExprT.Add
    mul = VarExprT.Mul

instance HasVars VarExprT where
    var = VarExprT.Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var s = M.lookup s

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit num = \_ -> Just num
    add f g = \m -> if isNothing (f m) || isNothing (g m) then
                        Nothing
                    else 
                        ust (fromJust (f m) + fromJust (g m))
    mul f g = \m -> if isNothing (f m) || isNothing (g m)) then
                        Nothing
                    else
                        Just (fromJust (f m) * fromJust (g m))

-- Test 
withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs expr = expr $ M.fromList vs

-- MY UNDERSTANDING ABOUT THIS EXECERCISE
-- Lit takes a integer and returns a functions which takes a map and
-- returns a Integer.
-- Add takes two arguments - both are functions. These functions take a map
-- and returns a integer. The output of add is also a functions which takes
-- a map and returns a integer. So we'll make the output function take the
-- map we recieve and then pass this map as the argument to the functions 
-- we received as argument add, this gives us the integers from them.

-- For eg: withVars [("x", 6)] $ add (lit 3) (var "x")
-- It get's converted to add (lit 3) (var "x") (map of x and 6)
-- Let's focus on add (lit 3) (var "x")
-- It can be written as add f g
-- From the below implementation we know that
-- lit num map == Just num, hence lit 3 == Just 3
-- From the instance of HasVars we have var s = M.lookup s, so we have
-- var "x" == M.lookup "x" (map of x and 6), this returns Just 6
-- Hence add (lit 3) (var "x" ) ==
--        ==  fromJust (lit 3) + fromJust (var "x")
--        ==  fromJust (Just 3) + fromJust (M.lookup "x" (map of x and 6) )
--        ==  fromJust (Just 3) + fromJust (Just 6)
--        ==  3 + 6 