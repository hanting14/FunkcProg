import Control.Monad

{- State -}

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap = liftM

instance Applicative (State s) where
  pure  = return
  (<*>) = ap

instance Monad (State s) where
  return a = State (\s -> (a, s))
  (State f) >>= g = State (\s -> case (f s) of (a, s') -> runState (g a) s')

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = do {s <- get; put (f s)}

evalState :: State s a -> s -> a
evalState ma = fst . runState ma

execState :: State s a -> s -> s
execState ma = snd . runState ma


{- Stack -}

type Stack a b = State [a] b

runStack :: Stack a b -> (b, [a])
runStack s = runState s []

evalStack :: Stack a b -> b
evalStack s = evalState s []

execStack :: Stack a b -> [a]
execStack s = execState s []


push :: a -> Stack a ()
push a = modify (a:)

pop :: Stack a a
pop = do {st <- get; case st of (a:as) -> do {put as; pure a}}

top :: Stack a a
top = head <$> get

-- Task 1: Extend the stack interface with an `isEmpty` function, which
--         returns `True` if and only if there are no elements in the stack.
--         Use the operations of the State monad! [get, put, modify]


		
isEmpty :: Stack a Bool
isEmpty = null <$> get

-- Task 2: Write a function that replaces the top two values in the stack with
--         the result of their multiplication if there are at least two numbers.
--         Otherwise the stack should be left intact.
--
--         Constraint: Pattern matching is not allowed, use the monadic
--                     Stack interface! [push, pop, top, isEmpty]

tryMul :: Stack Int ()
tryMul = do
  e <- isEmpty
  if e
    then do
		pure ()
    else do
		x <- pop
		e2 <- isEmpty
		if e2
		then do
			pure ()
		else do
			y <- pop
			push (x*y)

{-
  Some example evaluations:
  - evalStack (do {isEmpty}) == True
  - evalStack (do {push False; isEmpty}) == False
  - evalStack (do {push 'a'; push 'b'; pop; isEmpty}) == False
  - evalStack (do {push 'a'; pop; push 'b'; pop; isEmpty}) == True
  - evalStack (do {push 'a'; push 'b'; pop; pop; isEmpty}) == True

  - execStack (do {tryMul}) == []
  - execStack (do {push 5; tryMul}) == [5]
  - execStack (do {push 5; push 8; tryMul}) == [40]
  - execStack (do {push 5; push 8; push 2; tryMul}) == [16, 5]
  - execStack (do {push 5; pop; push 8; tryMul}) == [8]
-}