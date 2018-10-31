module StateMonad where
import Control.Monad (mapM_)

type State = Int

-- State Transformer
-- Remember: ST is another name for a function.
-- Remember: S is a ST constructor.
newtype ST a = S (State -> (a, State))

-- App gives us the function inside the state transformer ST.
-- It does this by taking a ST, pattern matching on the single constructor S, and then returning the wrapped function.
app :: ST a -> State -> (a, State)
app (S st) = st

instance Functor ST where
  fmap :: (a -> b) -> ST a -> ST b
  -- We're going from a (State -> (a, State)) to a (State -> (b, State)) using g :: (a -> b).
  fmap g (S st) = S (\s -> let (x, s') = st s in (g x, s'))

instance Applicative ST where
  pure :: a -> ST a
  pure x = S (\s -> (x, s))
  (<*>) :: ST (a -> b) -> ST a -> ST b
  -- Remember: ST (a -> b) is the same as (State -> ((a -> b), State)).
  -- We're taking a (State -> (a, State)) and making a (State -> (b, State)) using (State -> ((a -> b), State)).
  (<*>) (S g) (S h) = S (\s -> let (g', s') = g s
                                   (x, s'') = h s'
                                in (g' x, s''))

instance Monad ST where
  (>>=) :: ST a -> (a -> ST b) -> ST b
  --    :: S (State -> (a, State))       ->
  --       (a -> (State -> (b, State)))  ->
  --       S (State -> (b, State))
  (>>=) (S a) f = S (\s -> let (a', s') = a s in app (f a') s')

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- Non-monadic relabel: we manually thread the Int through the recursion.
rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _)   n = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
                      where
                        (l', n')  = rlabel l n
                        (r', n'') = rlabel r n'

relabeledTree1 :: Tree Int
relabeledTree1 = fst $ rlabel tree 0

-- Note the types (Tree a -> Int -> (Tree Int, Int)) and (Tree a -> ST (Tree Int)) are the same.
-- Monad relabel:

fresh :: ST Int
-- The state is Int.
fresh = S (\n -> (n, n+1))

alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _)   = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf _)   = do n <- fresh
                       return (Leaf n)
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')

extract :: ST a -> a
extract (S f) = let (a, _) = f 0 in a

relabeledTree2 :: Tree Int
relabeledTree2 = extract $ alabel tree

relabeledTree3 :: Tree Int
relabeledTree3 = extract $ mlabel tree

demo :: IO ()
demo = mapM_ print [relabeledTree1, relabeledTree2, relabeledTree3]
