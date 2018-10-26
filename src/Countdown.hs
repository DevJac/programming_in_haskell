module Countdown where

data Op = Add | Sub | Mul | Div deriving Eq

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

data Expr = Val Int | App Op Expr Expr

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

-- I'm not super confident this normalizes every equation form.
validExpr :: Expr -> Bool
validExpr (Val _) = True
-- No (e + 1), instead do (1 + e).
validExpr (App Add (App _ _ _) (Val _)) = False
validExpr (App Mul (App _ _ _) (Val _)) = False
-- No ((1 + 2) + e) or (e + (1 + 2)), instead do (1 + (2 + e)).
validExpr (App Add (App Add _ _) (App _ _ _)) = False
validExpr (App Mul (App Mul _ _) (App _ _ _)) = False
validExpr (App Add (App _ _ _) (App Add _ _)) = False
validExpr (App Mul (App _ _ _) (App Mul _ _)) = False
-- Smallest to largest for associative functions.
validExpr (App Add (Val l) (Val r)) | l > r = False
validExpr (App Mul (Val l) (Val r)) | l > r = False
validExpr (App Add (Val a) (App Add (Val b) _)) | a > b = False
validExpr (App Mul (Val a) (App Mul (Val b) _)) | a > b = False
validExpr (App Add (App _ (Val a) _) (App _ (Val b) _)) | a > b = False
validExpr (App Mul (App _ (Val a) _) (App _ (Val b) _)) | a > b = False
-- All sub-expressions must also be valid.
validExpr (App _ l r) = validExpr l && validExpr r


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
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n], validExpr e]
