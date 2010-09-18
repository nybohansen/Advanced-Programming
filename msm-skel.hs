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
haltWithError str = do s <- get
                       error (str ++ "\nFinal state: " ++ (show s))

-- | This function runs the MSM.
interp :: MSM ()
interp = do inst <- getInst
            cont <- check inst
            when cont $ do cont <- interpInst inst
                           when cont interp

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
                         return True

interpInst POP      = do s <- modify (\s -> s{stack = tail $ stack s})
                         return True
                         
interpInst DUP      = do s <- modify (\s -> case stack s of (x:xs)   -> s{stack = x : x : xs})
                         return True

interpInst SWAP     = do s <- modify (\s -> case stack s of (x:y:xs) -> s{stack = y : x : xs})
                         return True

interpInst LOAD_A   = do s <- modify (\s -> s{stack = (regA s) : stack s})
                         return True

interpInst LOAD_B   = do s <- modify (\s -> s{stack = (regB s) : stack s})
                         return True

interpInst STORE_A  = do s <- modify (\s -> case stack s of (x:xs)   -> s{stack = xs, regA = x})
                         return True
                         
interpInst STORE_B  = do s <- modify (\s -> case stack s of (x:xs)   -> s{stack = xs, regB = x})
                         return True
                         
interpInst NEG      = do s <- modify (\s -> case stack s of (x:xs)   -> s{stack = -x : xs})
                         return True
                         
interpInst ADD      = do s <- modify (\s -> case stack s of (x:y:xs) -> s{stack = (x+y) : xs})
                         return True
                         
interpInst JMP      = do s <- modify (\s -> case stack s of (x:xs)   -> s{stack = xs, pc = x})
                         return True
                         
interpInst (CJMP a) = do s <- modify (\s -> case stack s of (x:xs)   -> if x < 0
                                                                            then s{stack = xs, pc = a}
                                                                            else s{stack = xs, pc = pc s + 1})
                         return True
                                               
interpInst HALT     = return False

-- | This function is called in each step
-- It checks that the next instrution will be valid to run
check :: Inst -> MSM Bool
check inst = do s <- get
                if (pc s > (length $ prog s))
                    then haltWithError "Program counter went too far, perhaps you're missing a HALT"
                    else case inst of (PUSH _) -> return True
                                      SWAP     -> case stack s of (_:_:_)   -> return True
                                                                  otherwise -> haltWithError ((show inst) ++ " failed")
                                      LOAD_A   -> return True
                                      LOAD_B   -> return True
                                      ADD      -> case stack s of (_:_:_)   -> return True
                                                                  otherwise -> haltWithError ((show inst) ++ " failed")
                                      HALT     -> return True
                                      _        -> case stack s of (_:_)     -> return True
                                                                  otherwise -> haltWithError ((show inst) ++ " failed")
                
-- | This function constructs the initial state of an MSM running the
-- given program.
initial :: Prog -> State
initial p = State { prog = p, pc = 0, stack = [], regA = 0, regB = 0 }

-- | This function runs the given program on the MSM
runMSM :: Prog -> Maybe State
runMSM p = let (MSM f) = interp 
           in fmap snd $ f $ initial p
           
-- Will result in a state with one item on the stack, namely 42
test1 = runMSM [PUSH 12, PUSH 22, ADD, HALT]

-- Will result in an error regarding pop from empty stack
test2 = runMSM [POP]

-- Will result in an error regarding PC to large
test3 = runMSM [PUSH 1]

-- Calculates the nth fibonacci number
fibonacci n = 
    runMSM [-- Start of fibonacci numbers
            PUSH 1,
            PUSH 1,
            
            -- Subtract two, as we already have them
            PUSH n,
            PUSH 2,
            NEG,
            ADD,
            
            DUP,
            STORE_A,
            
            -- Continue the algorithm?
            CJMP 9,
            
            -- Stop calculating
            HALT,
            
            -- Start of the algorithm
            DUP,
            STORE_B,
            ADD,
            LOAD_B,
            SWAP,
            
            -- Subtract one from loop counter
            LOAD_A,
            PUSH 1,
            NEG,
            ADD,
            DUP,
            STORE_A,
            
            -- Jump to start of algorithm
            PUSH 8,
            JMP]