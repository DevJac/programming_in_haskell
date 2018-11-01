module Parser where
import Control.Applicative (Alternative(..), some, many)
import Data.Char ( isDigit
                 , isLower
                 , isUpper
                 , isAlpha
                 , isAlphaNum
                 , isSpace
                 )

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) s = p s

item :: Parser Char
item = P (\case []     -> []
                (c:cs) -> [(c, cs)])

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  --   :: (a -> b)
  --   -> P (String -> [(a, String)])
  --   -> P (String -> [(b, String)])
  fmap f p = P (\s -> [(f c, cs) | (c, cs) <- parse p s])

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = P (\s -> [(a, s)])
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  --    :: P (String -> [((a -> b), String)])
  --    -> P (String -> [(a, String)])
  --    -> P (String -> [(b, String)])
  pf <*> pa = P (\s -> do (f, s')  <- parse pf s
                          (a, s'') <- parse pa s'
                          return (f a, s''))
  -- Alternative implementation, without do notation:
  -- pf <*> pa = P (\s -> parse pf s  >>= \(f, s' ) ->
  --                      parse pa s' >>= \(a, s'') ->
  --                      pure (f a, s''))
  -- (>>=) for list is: a >>= fb = concat (fmap fb a)

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  --    :: P (String -> [(a, String)])
  --    -> (a -> (P (String -> [(b, String)])))
  --    -> P (String -> [(b, String)])
  pa >>= fb = P (\s -> do (a, s') <- parse pa s
                          parse (fb a) s')

instance Alternative Parser where
  empty :: Parser a
  empty = P (\_ -> [])
  (<|>) :: Parser a -> Parser a -> Parser a
  --    :: P (String -> [(a, String)])
  --    -> P (String -> [(a, String)])
  --    -> P (String -> [(a, String)])
  pa <|> pb = P (\s -> case parse pa s of []     -> parse pb s
                                          parsed -> parsed)

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then pure x else empty

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string [] = pure []
string (c:cs) = do _ <- char c
                   _ <- string cs
                   return (c:cs)

-- some and many are provided by the Alternative typeclass.

ident :: Parser String
ident = do c <- lower
           cs <- many alphanum
           return (c:cs)

nat :: Parser Int
nat = do d <- some digit
         return (read d)

int :: Parser Int
int = nat <|> do _ <- char '-'
                 n <- nat
                 return (-n)

space :: Parser ()
space = do _ <- many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = space *> p <* space

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol = token . string

-- list may not work as expected with parsers that consume commas (,).
list :: Parser a -> Parser [a]
list p = do _ <- symbol "["
            n <- p
            ns <- many (do _ <- symbol ","
                           p)
            _ <- symbol "]"
            return (n:ns)

nats :: Parser [Int]
nats = list natural

ints :: Parser [Int]
ints = list integer
