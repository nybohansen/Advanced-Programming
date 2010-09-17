{-
    Course: Advanced Programming
    Authors: Kasper Nybo Hansen and Mads Ohm Larsen
-}
module MSM where

-- we want to use monads here
import Control.Monad

-- | This data type represents instructions for the MSM.
data Inst = PUSH Int
          | POP
          | DUP
          | SWAP
          | LOAD_A
          | LOAD_B
          | STORE_A
          | STORE_B
          | NEG
          | ADD 
          | JMP
          | CJMP Int
          | HALT 
          deriving (Eq, Show)
 
-- | This type represents programs for the MSM.
type Prog = [Inst]

-- | This type represents the stack of the MSM.
type Stack = [Int]

-- | This data type encapsulates the state of a running MSM.
data State = State { prog  :: Prog
                   , pc    :: Int
                   , stack :: Stack
                   , regA  :: Int
                   , regB  :: Int 
                   } deriving (Show)


-- This is the monad that is used to implement the MSM. 
newtype MSM a = MSM (State -> Maybe (a, State))

instance Monad MSM where
    -- (>>=) :: MSM a -> (a -> MSM b) -> MSM b
    (MSM p) >>= k = MSM (\s -> let Just (result, newS) = p s
                                   MSM p' = k result
                               in p' newS)

    -- return :: a -> MSM a
    return a = MSM (\s -> Just (a, s))


-- The following four functions provide an interface to implement
-- operations of the MSM.

-- | This function returns the current state of the running MSM.
get :: MSM State
get = MSM (\s -> Just (s, s))

-- | This function set a new state for the running MSM.
set :: State -> MSM ()
set m = MSM (\_ -> Just ((), m))

-- | This function modifies the state for the running MSM according to
-- the provided function argument
modify :: (State -> State) -> MSM ()
modify f = do s <- get
              set (f s) 

-- | This function halts the execution with an error.
haltWithError :: String -> MSM a
haltWithError s = error s

-- | This function runs the MSM.
interp :: MSM ()
interp = run 
         where run = do inst <- getInst
                        cont <- interpInst inst
                        when cont run

-- This function returns the next instruction on the stack
getInst :: MSM Inst
getInst = do s    <- get
             newS <- modify (\s -> s{pc = pc s + 1})
             return ((prog s) !! (pc s))

-- | This function interprets the given instruction. It returns True
-- if the MSM is supposed to continue it's execution after this
-- instruction.
interpInst :: Inst -> MSM Bool
interpInst (PUSH a) = do s <- modify (\s -> s{stack = a : stack s})
                         return stackControl

interpInst POP      = do s <- modify (\s -> case stack s of (x:xs)    -> s{stack = xs}
                                                            otherwise -> seq (haltWithError "POP error") s)
                         return True
                         
interpInst DUP      = do s <- modify (\s -> case stack s of (x:xs)    -> s{stack = x : x : xs}
                                                            otherwise -> seq (haltWithError "DUP error") s)
                         return True

interpInst SWAP     = do s <- modify (\s -> case stack s of (x:y:xs)  -> s{stack = y : x : xs}
                                                            otherwise -> seq haltWithError "SWAP error" s)
                         return True

interpInst LOAD_A   = do s <- modify (\s -> s{stack = (regA s) : stack s})
                         return True

interpInst LOAD_B   = do s <- modify (\s -> s{stack = (regB s) : stack s})
                         return True

interpInst STORE_A  = do s <- modify (\s -> case stack s of (x:xs)    -> s{stack = xs, regA = x}
                                                            otherwise -> seq (haltWithError "STORE A error") s)
                         return True
                         
interpInst STORE_B  = do s <- modify (\s -> case stack s of (x:xs)    -> s{stack = xs, regB = x}
                                                            otherwise -> seq (haltWithError "STORE B error") s)
                         return True
                         
interpInst NEG      = do s <- modify (\s -> case stack s of (x:xs)    -> s{stack = -x : xs}
                                                            otherwise -> seq (haltWithError "NEG error") s)
                         return True
                         
interpInst ADD      = do s <- modify (\s -> case stack s of (x:y:xs)  -> s{stack = (x+y) : xs}
                                                            otherwise -> seq (haltWithError "ADD") s)
                         return True
                         
interpInst JMP      = do s <- modify (\s -> case stack s of (x:xs)    -> s{stack = xs, pc = x}
                                                            otherwise -> seq (haltWithError "JMP") s)
                         return True
                         
interpInst (CJMP a) = do s <- modify (\s -> case stack s of (x:xs)    -> if x > 0
                                                                         then s{stack = xs, pc = a}
                                                                         else s{stack = xs}
                                                            otherwise -> seq (haltWithError "CJMP") s)
                         return True
                                               
interpInst HALT     = return False


{-

--Couldn't we get the state by using the get function defined??
stackControl :: Bool
stackControl = stackControl2 get 

-- Don't know what i'm doing ;-)                
stackControl2 :: MSM State -> Bool
stackControl2 state = True
-}
                
-- | This function constructs the initial state of an MSM running the
-- given program.
initial :: Prog -> State
initial p = State { prog = p, pc = 0, stack = [], regA = 0, regB = 0 }

-- | This function runs the given program on the MSM
runMSM :: Prog -> Maybe State
runMSM p = let (MSM f) = interp 
           in fmap snd $ f $ initial p