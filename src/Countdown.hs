module Countdown where
import Data.List

data Op = Add | Sub | Mul | Div deriving (Eq, Ord)

instance Show Op where
  show :: Op -> String
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

data Expr = Val Int | App Op Expr Expr deriving (Eq, Ord)

instance Show Expr where
  show (Val n)     = show n
  show (App o l r) = opShow o l ++ show o ++ opShow o r
                     where
                       opShow _       v@(Val _)           = show v
                       opShow outerOp e@(App innerOp _ _) = if innerOp /= outerOp
                                                            then "(" ++ show e ++ ")"
                                                            else show e

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1
valid Div x y = y /= 1 && x `mod` y == 0

rewrite :: Expr -> Expr
-- Simple case
rewrite (Val n) = (Val n)
-- Add rules
rewrite (App Add l@(App _ _ _) r@(Val _)) = rewrite (App Add (rewrite r) (rewrite l))
rewrite (App Add (Val l) (App Add (Val r) e)) | l > r = rewrite (App Add (Val r) (App Add (Val l) (rewrite e)))
rewrite (App Add (Val l) (App Sub r e)) = rewrite (App Sub (App Add (Val l) (rewrite r)) (rewrite e))
rewrite (App Add (App Add a b) r) = rewrite (App Add (rewrite a) (App Add (rewrite b) (rewrite r)))
rewrite (App Add l@(App _notAdd _ _) r@(App Add _ _)) = rewrite (App Add (rewrite r) (rewrite l))
rewrite (App Add l r) | l > r = rewrite (App Add (rewrite r) (rewrite l))
-- Mul rules
rewrite (App Mul l@(App _ _ _) r@(Val _)) = rewrite (App Mul (rewrite r) (rewrite l))
rewrite (App Mul (Val l) (App Mul (Val r) e)) | l > r = rewrite (App Mul (Val r) (App Mul (Val l) (rewrite e)))
rewrite (App Mul (Val l) (App Div r e)) = rewrite (App Div (App Mul (Val l) (rewrite r)) (rewrite e))
rewrite (App Mul (App Mul a b) r) = rewrite (App Mul (rewrite a) (App Mul (rewrite b) (rewrite r)))
rewrite (App Mul l@(App _notAddOrMul _ _) r@(App Mul _ _)) = rewrite (App Mul (rewrite r) (rewrite l))
rewrite (App Mul l r) | l > r = rewrite (App Mul (rewrite r) (rewrite l))
-- The rest
rewrite e@(App o l r) | needsRewrite e = rewrite (App o (rewrite l) (rewrite r))
                      | otherwise      = e

needsRewrite :: Expr -> Bool
-- Simple case
needsRewrite (Val _) = False
-- Add rules
needsRewrite (App Add (App _ _ _) (Val _)) = True
needsRewrite (App Add (Val l) (App Add (Val r) _)) | l > r = True
needsRewrite (App Add (Val _) (App Sub _ _)) = True
needsRewrite (App Add (App Add _ _) _) = True
needsRewrite (App Add (App _notAdd _ _) (App Add _ _)) = True
needsRewrite (App Add l r) | l > r = True
-- Mul rules
needsRewrite (App Mul (App _ _ _) (Val _)) = True
needsRewrite (App Mul (Val l) (App Mul (Val r) _)) | l > r = True
needsRewrite (App Mul (Val _) (App Div _ _)) = True
needsRewrite (App Mul (App Mul _ _) _) = True
needsRewrite (App Mul (App _notAddOrMul _ _) (App Mul _ _)) = True
needsRewrite (App Mul l r) | l > r = True
-- The rest
needsRewrite (App _ l r) = needsRewrite l || needsRewrite r

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n]
eval (App o l r) = [ apply o x y | x <- eval l,
                                   y <- eval r,
                                   valid o x y]

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = map (x:) xs' ++ xs'
              where xs' = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave a (x:xs) = (a : x : xs) : (map (x:) (interleave a xs))

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

split :: [a] -> [([a], [a])]
split []     = []
split [_]    = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r |  o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
  elem (values e) (choices ns) && eval e == [n]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = nub [rewrite e | ns' <- choices ns, e <- exprs ns', eval e == [n]]
