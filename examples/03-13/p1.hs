data Counter a = Counter a Integer
  deriving (Show,Eq)

instance Functor Counter where
  fmap f (Counter a i) = Counter (f a) i

instance Applicative Counter where
  pure x = Counter x 0
  Counter f i <*> Counter x j = Counter (f x) (i + j)

instance Monad Counter where
  return = pure
  (Counter x i) >>= f =
    let Counter y j = f x
     in Counter y (i + j + 1)

add c1 c2 =
  do x <- c1     --  c1 >>= (\x -> ... )
     y <- c2
     return (x + y)

newtype Parser t = Parser (String -> [(t,String)])
run (Parser p) = p


char s =
  Parser (\inp -> case inp of
                  (s':ss) | s == s' -> [(s,ss)]
                  otherwise         -> [])
oneOf xx =
  Parser (\inp -> case inp of
                  (s:ss) | s `elem` xx -> [(s,ss)]
                  otherwise            -> [])

sat pred =
  Parser (\inp -> case inp of
                  (s:ss) | pred s    -> [(s,ss)]
                  otherwise          -> [])

digit = sat (\v -> v >= '0' && v <= '9')

p1 = run (oneOf "abc")  "axy"
p2 = run (oneOf "abc")  "xya"

instance Functor Parser where
  fmap f (Parser  p1) =
      Parser (\inp -> [(f t, s) |
                       (t,s) <- p1 inp])

instance Applicative Parser where
  pure a = Parser (\inp -> [(a,inp)])
  (Parser p1) <*> (Parser p2) =
      Parser (\inp -> [(v1 v2, ss2) |
                       (v1,ss1) <- p1 inp,
                       (v2,ss2) <- p2 ss1])

instance Monad Parser where
  (Parser p) >>= f =
      Parser (\inp -> concat [run (f v) inp'
                             | (v,inp') <- p inp])

data Exp = IntExp Integer
         | PlusExp Exp Exp
  deriving Show

getInteger :: Parser Integer
getInteger = Parser (\inp -> case run digit inp of
                       [(d, dd)] -> [(read [d], dd)]
                       otherwise -> [])

getActualInteger :: Parser Integer
getActualInteger = do digits <- many1 digit
                      return (read digits)

getIntExp = IntExp <$> sdi

sdi :: Parser Integer
sdi = Parser (\inp -> case run digit inp of
                       [(d, dd)] -> [(read [d], dd)]
                       otherwise -> [])

p3 = run (IntExp <$> sdi)  "123"
p4 = run (PlusExp <$> getIntExp <*> getIntExp)  "123"

p5 :: Parser Exp
p5 = do i1 <- getIntExp
        i2 <- getIntExp
        return (PlusExp i1 i2)

p5' = getIntExp >>= (\i1 ->
      getIntExp >>= (\i2 -> return $ PlusExp i1 i2))

(Parser p1) <|> (Parser p2) =
   Parser (\inp -> take 1 $ p1 inp ++ p2 inp)

string [] = Parser (\inp -> [([],inp)])
string (s:ss) = do v <- char s
                   vv <- string ss
                   return $ v:vv

getPlusExp = do string "+"
                e1 <- getExp
                e2 <- getExp
                return (PlusExp e1 e2)

reject = Parser (\inp -> [])

many p = next <|> return []
   where next = do v <- p
                   vv <- many p
                   return (v:vv)

many1 p = do v <- p
             vv <- many p
             return (v:vv)


getExp = getIntExp 
     <|> getPlusExp
