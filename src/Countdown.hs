module Countdown where
import Data.List

data Op = Add | Sub | Mul | Div deriving Eq

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

data Expr = Val Int | App Op Expr Expr deriving Eq

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
rewrite = rewrite' True

rewrite' :: Bool -> Expr -> Expr
-- Simple case
rewrite' _ (Val n) = (Val n)
-- Add rules
rewrite' c (App Add (Val l) (Val r)) | l > r = rewrite' c (App Add (Val r) (Val l))
rewrite' c (App Add l@(App _ _ _) r@(Val _)) = rewrite' c (App Add (rewrite' c r) (rewrite' c l))
rewrite' c (App Add (Val l) (App Add (Val r) e)) | l > r = rewrite' c (App Add (Val r) (App Add (Val l) (rewrite' c e)))
rewrite' c (App Add (Val l) (App Sub (Val r) e)) = rewrite' c (App Sub (App Add (Val l) (Val r)) (rewrite' c e))
rewrite' c (App Add (App Add a b) r) = rewrite' c (App Add (rewrite' c a) (App Add (rewrite' c b) (rewrite' c r)))
rewrite' c (App Add l@(App _notAdd _ _) r@(App Add _ _)) = rewrite' c (App Add (rewrite' c r) (rewrite' c l))
-- Mul rules
rewrite' c (App Mul (Val l) (Val r)) | l > r = rewrite' c (App Mul (Val r) (Val l))
rewrite' c (App Mul l@(App _ _ _) r@(Val _)) = rewrite' c (App Mul (rewrite' c r) (rewrite' c l))
rewrite' c (App Mul (Val l) (App Mul (Val r) e)) | l > r = rewrite' c (App Mul (Val r) (App Mul (Val l) (rewrite' c e)))
rewrite' c (App Mul (Val l) (App Div (Val r) e)) = rewrite' c (App Div (App Mul (Val l) (Val r)) (rewrite' c e))
rewrite' c (App Mul (App Mul a b) r) = rewrite' c (App Mul (rewrite' c a) (App Mul (rewrite' c b) (rewrite' c r)))
rewrite' c (App Mul l@(App _notAddOrMul _ _) r@(App Mul _ _)) = rewrite' c (App Mul (rewrite' c r) (rewrite' c l))
-- The rest
rewrite' True (App o l r) = rewrite' False (App o (rewrite' True l) (rewrite' True r))
rewrite' False e = e

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
