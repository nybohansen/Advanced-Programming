{-
    Course: Advanced Programming
    Author: Mads Ohm Larsen
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
data State = State { prog  :: Prog,
                     pc    :: Int,
                     stack :: Stack,
                     regA  :: Int,
                     regB  :: Int }
           deriving (Show)


-- This is the monad that is used to implement the MSM. 
newtype MSM a = MSM (State -> Maybe (a, State))

instance Monad MSM where
    -- (>>=) :: MSM a -> (a -> MSM b) -> MSM b
    (MSM p) >>= k = k p

    -- return :: a -> MSM a
    return a = 


-- The following four functions provide an interface to implement
-- operations of the MSM.

-- | This function returns the current state of the running MSM.
get :: MSM State
get = ...

-- | This function set a new state for the running MSM.
set :: State -> MSM ()
set m = ...

-- | This function modifies the state for the running MSM according to
-- the provided function argument
modify :: (State -> State) -> MSM ()
modify f = ...

-- | This function halts the execution with an error.
haltWithError :: MSM a
haltWithError = ...

-- | This function runs the MSM.
interp :: MSM ()
interp = run 
         where run = do inst <- getInst
                        cont <- interpInst inst
                        when cont run

-- | This function interprets the given instruction. It returns True
-- if the MSM is supposed to continue it's execution after this
-- instruction.
interpInst :: Inst -> MSM Bool
--interpInst (PUSH a) = 
--interpInst POP      =
--interpInst DUP      =
--interpInst SWAP     =
--interpInst LOAD_A   =
--interpInst LOAD_B   =
--interpInst STORE_A  =
--interpInst STORE_B  =
--interpInst NEG      =
interpInst ADD      = do x <- get
                         y <- get
                         add 
--interpInst JMP      =
--interpInst (CJMP a) =
--interpInst HALT     =
--interpInst _        = 

add :: Int -> Int -> Int
add (Num x) (Num y) = return $ (x + y)

-- | This function constructs the initial state of an MSM running the
-- given program.
initial :: Prog -> State
initial p = State { prog = p, pc = 0, stack = [], regA = 0, regB = 0 }

-- | This function runs the given program on the MSM
runMSM :: Prog -> Maybe State
runMSM p = let (MSM f) = interp 
           in fmap snd $ f $ initial p