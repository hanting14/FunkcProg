{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Bool

-- State monad
--------------------------------------------------------------------------------

newtype State s a = State {runState :: s -> (a, s)}
  deriving Functor

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State $ \s -> (a, s)
  State f >>= g = State $ \s -> case f s of
    (a, s') -> runState (g a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = get >>= \s -> put (f s)

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma

--------------------------------------------------------------------------------
--                              Feladatok
--------------------------------------------------------------------------------

data List a b
  = Nil
  | Cons a b (List a b)
  deriving (Show)

instance (Eq a, Eq b) => Eq (List a b) where
	(==) Nil Nil = True
	(==) (Cons la lb lx) (Cons ra rb ry) = ((la == ra) && (lb == rb) && (lx == ry) )
	(==) _ _ = False

instance Functor (List a) where
  fmap f Nil = Nil
  fmap f (Cons a b x) = (Cons a (f b) (fmap f x))

instance Foldable (List a) where
  foldr f acc Nil = acc
  foldr f acc (Cons a b x) = f b (foldr f acc x)

instance Traversable (List a) where
  traverse f Nil = pure Nil
  traverse f (Cons a b x) = Cons a <$> f b <*> traverse f x

unpack :: List a b -> ([a], [b])
unpack Nil = ([], [])
unpack (Cons a b x) = (a:(fst(unpack x)), (b:(snd(unpack x))))

pack :: ([a], [b]) -> List a b
pack (a:as, b:bs) = Cons a b $ pack (as, bs)
pack (_, _)       = Nil

reverseAs :: List a b -> List a b
reverseAs ls = pack (reverse as, bs)
  where
    (as, bs) = unpack ls

data Tree a b
  = Leaf  a b
  | Node (Tree a b) (Tree a b)
  deriving (Eq, Show)

extend :: List a b -> List a b -> List a b
extend (Cons a b Nil) ls = Cons a b ls
extend (Cons a b l)   ls = Cons a b $ extend l ls
extend Nil            ls = ls

toList :: Tree a b -> List a b
toList (Leaf a b) = Cons a b Nil
toList (Node l r) = toList l `extend` toList r


labelAs :: Tree a b -> Tree (a, Int) b
labelAs t = evalState (go t) 0 
  where
    go :: Tree a b -> State Int (Tree (a, Int) b)
    go (l `Node` r) = do
      l <- go l
      r <- go r
      pure $ l `Node` r
    go (a `Leaf` b) = do
      n <- get 
      put $ n + 1
      pure $ Leaf (a, n) b


--------------------------------------------------------------------------------
--                  While nyelv parser + interpreter
--------------------------------------------------------------------------------

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}
  deriving Functor

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return a = Parser $ \s -> Just (a, s)
  Parser f >>= g = Parser $ \s ->
    case f s of
      Nothing     -> Nothing
      Just(a, s') -> runParser (g a) s'

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (<|>) (Parser f) (Parser g) = Parser $ \s -> case f s of
    Nothing -> g s
    x       -> x

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
  c:cs | f c -> Just (c, cs)
  _          -> Nothing

eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((), [])
  _  -> Nothing

char :: Char -> Parser ()
char c = () <$ satisfy (==c)

string :: String -> Parser ()
string = mapM_ char

ws :: Parser ()
ws = () <$ many (char ' ' <|> char '\n')

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 pa pb = (:) <$> pa <*> many (pb *> pa)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy pa pb = sepBy1 pa pb <|> pure []

anyChar :: Parser Char
anyChar = satisfy (const True)

-- While nyelv
------------------------------------------------------------

data Exp
  = Add Exp Exp    -- a + b
  | Mul Exp Exp    -- a * b
  | Var Name       -- x
  | IntLit Int
  | BoolLit Bool   -- true|false
  | Not Exp        -- not e
  | And Exp Exp    -- a && b
  | Or Exp Exp     -- a || b
  | Eq Exp Exp     -- a == b
  | Lt Exp Exp     -- a < b
  | Pair Exp Exp   -- (a, b)
  | Fst Exp        -- (a, b) -> a
  | Snd Exp        -- (a, b) -> b
  deriving (Eq, Show)

type Program = [Statement]
type Name    = String

data Statement
  = Assign Name Exp         -- x := e
  | While Exp Program       -- while e do p1 end
  | If Exp Program Program  -- if e then p1 else p2 end
  | Block Program           -- {p1}  (lok??lis scope)
  deriving (Eq, Show)


-- While parser
--------------------------------------------------------------------------------

{-
Parser a While nyelvhez. A szintaxist az Exp ??s Statement defin??ci??n??l l??taht??
fenti kommentek ??sszegzik, tov??bb??:

  - mindenhol lehet whitespace tokenek k??z??tt
  - a Statement-eket egy Program-ban v??lassza el ';'
  - Az oper??torok er??ss??ge ??s assszociativit??sa a k??vetkez?? (cs??kken?? er??ss??gi sorrendben):
      *  (bal asszoc)
      +  (bal asszoc)
      <  (nem asszoc)
      == (nem asszoc)
      && (jobb asszoc)
      || (jobb asszoc)
  - "not" er??sebben k??t minden oper??torn??l.
  - A kulcsszavak: not, and, while, do, if, end, true, false.
  - A v??ltoz??nevek legyenek bet??k olyan nem??res sorozatai, amelyek *nem* kulcsszavak.
    Pl. "while" nem azonos??t??, viszont "whilefoo" m??r az!

P??lda szintaktikilag helyes programra:

  x := 10;
  y := x * x + 10;
  while (x == 0) do
    x := x + 1;
    b := true && false || not true
  end;
  z := x
-}

char' :: Char -> Parser ()
char' c = char c <* ws

string' :: String -> Parser ()
string' s = string s <* ws

keywords :: [String]
keywords = ["not", "and", "while", "do", "if", "end", "true", "false", "fst", "snd"]

pIdent :: Parser String
pIdent = do
  x <- some (satisfy isLetter) <* ws
  if elem x keywords
    then empty
    else pure x

pBoolLit :: Parser Bool
pBoolLit = (True  <$ string' "true")
       <|> (False <$ string' "false")

pIntLit :: Parser Int
pIntLit = read <$> (some (satisfy isDigit) <* ws)

pAtom :: Parser Exp
pAtom = (BoolLit <$> pBoolLit)
      <|> (IntLit <$> pIntLit)
      <|> (Var <$> pIdent)
      <|> (char' '(' *> (pPair <|> pExp) <* char' ')')

pPair :: Parser Exp
pPair = Pair <$> pExp <*> (char' ',' *> pExp)

pNot :: Parser Exp
pNot =
      (Not <$> (string' "not" *> pAtom))
  <|> (Fst <$> (string' "fst" *> pAtom))
  <|> (Snd <$> (string' "snd" *> pAtom))
  <|> pAtom
 
pMul :: Parser Exp
pMul = foldl1 Mul <$> sepBy1 pNot (char' '*')

pAdd :: Parser Exp
pAdd = foldl1 Add <$> sepBy1 pMul (char' '+')

pEqOrLt :: Parser Exp
pEqOrLt =
  pAdd >>= \e ->
    (Eq e <$> (string' "==" *> pAdd))
      <|> (Lt e <$> (string' "<" *> pAdd))
      <|> pure e

pAnd :: Parser Exp
pAnd = foldr1 And <$> sepBy1 pEqOrLt (string' "&&")

pOr :: Parser Exp
pOr = foldr1 Or <$> sepBy1 pAnd (string' "||")

pExp :: Parser Exp
pExp = pOr

pProgram :: Parser Program
pProgram = sepBy pStatement (char' ';')

pStatement :: Parser Statement
pStatement =
        (Assign <$> pIdent <*> (string' ":=" *> pExp))
    <|> (While <$> (string' "while" *> pExp)
               <*> (string' "do" *> pProgram <* string' "end"))
    <|> (If <$> (string' "if"   *> pExp)
            <*> (string' "then" *> pProgram)
            <*> (string' "else" *> pProgram <* string' "end"))
    <|> (Block <$> (char' '{' *> pProgram <* char' '}'))

pSrc :: Parser Program
pSrc = ws *> pProgram <* eof

-- Interpreter
------------------------------------------------------------

{-
Interpreter a While nyelvhez.

Kifejez??sek:
  - A logikai ??s artimetikai m??veletek ki??rt??kel??se ??rtelemszer??. Ha nem
    megfelel?? t??pus?? ??rt??ket kapunk argumentumokra, dobjunk "error"-al hib??t.
  - Az == oper??tor m??k??dik, ha mindk??t argumentum Bool, vagy ha mindk??t argumentum
    Int, az eredm??ny mindig Bool.

V??ltoz?? scope ??s ??rt??kad??s kezel??se:
  - ??j scope-nak sz??m??t:
    - minden "while" kifejez??s teste
    - minden "if" kifejez??s k??t ??ga
    - minden ??j Block (a szintaxisban pl "x := 0; {y := x; x := x}"-n??l
      a kapcsos z??r??jeles utas??t??ssorozat ??j blokkban van).

  - ha egy ??j v??ltoz??nak ??rt??ket adunk, akkor felvessz??k a k??rnyezet elej??re
  - ha egy megl??v?? v??ltoz??nak ??rt??ket adunk, akkor update-elj??k a v??ltoz?? ??rt??k??t
  - amikor az interpreter v??gez egy scope ki??rt??kel??s??val, eldobja az ??sszes,
    scope-ban ??jonnan felvett v??ltoz??t a k??rnyezetb??l.
-}

--type Val  = Either Int Bool
data Val = I Int | B Bool | P Val Val
  deriving (Eq, Show)
  
type Env  = [(Name, Val)]
type Eval = State Env

evalExp :: Exp -> Eval Val
evalExp e = case e of
  Add e1 e2 -> do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
      (I n1, I n2) -> pure (I (n1 + n2))
      _                  -> error "type error in + argument"
  Mul e1 e2 -> do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
      (I n1, I n2) -> pure (I (n1 * n2))
      _                  -> error "type error in * argument"
  Or e1 e2 -> do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
      (B b1, B b2) -> pure (B (b1 || b2))
      _                    -> error "type error in || argument"
  And e1 e2 -> do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
      (B b1, B b2) -> pure (B (b1 && b2))
      _                    -> error "type error in && argument"
  Eq  e1 e2 -> do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
      (I  n1, I  n2) -> pure (B (n1 == n2))
      (B b1, B b2) -> pure (B (b1 == b2))
      _                    -> error "type error in == arguments"
  Lt  e1 e2 -> do
    v1 <- evalExp e1
    v2 <- evalExp e2
    case (v1, v2) of
      (I  n1, I  n2) -> pure (B (n1 < n2))
      _                    -> error "type error in < arguments"
  Var x -> do
    env <- get
    case lookup x env of
      Nothing -> error ("variable not in scope: " ++ x)
      Just v  -> pure v
  IntLit n ->
    pure (I n)
  BoolLit b ->
    pure (B b)
  Not e -> do
    v <- evalExp e
    case v of
      B b -> pure (B (not b))
      _       -> error "type error in \"not\" argument"
  Pair f s -> do
    f <- evalExp f
    s <- evalExp s
    pure (P f s)
  Fst t -> do
    t <- evalExp t
    case t of
      P f _ -> pure f
      _     -> error "type error in \"fst\" argument"
  Snd t -> do
    t <- evalExp t
    case t of
      P _ s -> pure s
      _     -> error "type error in \"snd\" argument"

newScope :: Eval a -> Eval a
newScope ma = do
  env <- (get :: State Env Env)
  a <- ma
  modify (\env' -> drop (length env' - length env) env')
  pure a

updateEnv :: Name -> Val -> Env -> Env
updateEnv x v env =
  case go env of
    Nothing   -> (x, v):env
    Just env' -> env'
  where
    go :: Env -> Maybe Env
    go [] = Nothing
    go ((x', v'):env)
      | x == x'   = Just ((x, v):env)
      | otherwise = ((x', v'):) <$> go env

evalSt :: Statement -> Eval ()
evalSt s = case s of
  Assign x e -> do
    v <- evalExp e
    modify (updateEnv x v)
  While e p -> do
    v <- evalExp e
    case v of
      B b -> if b then newScope (evalProg p) >> evalSt (While e p)
                      else pure ()
      I _  -> error "type error: expected a Bool condition in \"while\" expression"
  If e p1 p2 -> do
    v <- evalExp e
    case v of
      B b -> if b then newScope (evalProg p1)
                      else newScope (evalProg p2)
      I _ -> error "type error: expected a Bool condition in \"if\" expression"
  Block p ->
    newScope (evalProg p)

evalProg :: Program -> Eval ()
evalProg = mapM_ evalSt


-- interpreter
--------------------------------------------------------------------------------

runProg :: String -> Env
runProg str = case runParser pSrc str of
  Nothing     -> error "parse error"
  Just (p, _) -> execState (evalProg p) []

p1 :: String
p1 = "x := 10; y := 20; {z := x + y}; x := x + 100"