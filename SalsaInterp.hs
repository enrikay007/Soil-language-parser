--
-- Skeleton for Salsa interpreter
-- To be used at the exam for Advanced Programming, B1-2013
--

module SalsaInterp
       -- (Position, interpolate, runProg)
where

import SalsaAst
import Gpx
import Data.Char(isSpace,isDigit)
import qualified Data.Map as Map
import Control.Monad

--
-- The function interpolate
--
data Sal = Sal { pos :: Position, ident :: [Ident], angle :: Integer}
           deriving (Show)

data SalsaState = SalsaState {salsa :: Sal,  animation :: Animation}
                deriving (Show)

type SalsaStates = [SalsaState]

type Position = (Integer, Integer)
interpolate :: Integer -> Position -> Position -> [Position]
interpolate = undefined
--
-- Define the types Context and SalsaCommand
--

data Context = Context {state :: SalsaStates, stack :: Stack, h_env :: Map.Map String Definition}
               deriving (Show)

newtype SalsaCommand a = SalsaCommand {runSC :: Context -> (a, Context) }

instance Monad SalsaCommand where 
  (SalsaCommand t) >>= f = SalsaCommand $ \c -> 
    case t c of
      (x, c') -> let (SalsaCommand np) = f x
                 in np c'
    
  return a = SalsaCommand $ \c -> (a, c)

-- functions for manipulating the context
type Environment = Map.Map String Integer

m_empty_environment :: Environment
m_empty_environment = Map.fromList([])  

type Stack = [Environment]


-- helper functions
          
get :: SalsaCommand Context
get = SalsaCommand $ \a -> (a,a)

edit :: Context -> SalsaCommand ()
edit pa = SalsaCommand $ \a -> ((), pa)

edit2 :: Context -> SalsaCommand (Context, Context) 
edit2 pa = SalsaCommand $ \a -> ((a,pa), pa) 


change :: (Context -> Context) -> SalsaCommand ()
change f = do {env <- get;                 
               edit ( f env  )}

change2 :: (Context -> Context) -> SalsaCommand (Context, Context) 
change2 f = do {env <- get;                 
                edit2 ( f env  )}


change_state f = change $ \a -> 
  let old_state = state a
  in a {state = f(old_state)}

change_state2 f = change2 $ \a -> 
  let old_state = state a
  in a {state = f(old_state)}


--
-- Define the function command
--

--command :: Command -> SalsaCommand ()


command :: Command -> SalsaCommand ()
command cmd = case cmd of 
-- Define the type Salsa
--
	Move name es -> undefined
	At b e->undefined
	Par a b-> undefined 
--data Salsa a = 


newtype Salsa a = Salsa {runDefCom :: Context -> (a, Context)}

instance Monad Salsa where 
  (Salsa i) >>= f = Salsa $ \e -> 
    case i e of
      (x, e') -> let (Salsa np) = f x
                 in np e'
    
  return a = Salsa $ \e -> (a, e)
                                  
-- Define the functions liftC, definition, and defCom
						
definition :: Definition -> Salsa ()               
definition def = liftC $ change $ \env -> undefined
                         -- let pname = name def
                             -- cmds  = body def
                             -- nh_env = Map.insert pname def (h_env env)
                         -- in env {h_env = nh_env}
						
move:: [Pos]-> [Ident]-> SalsaCommand()
move name [] = do return ()  
move name (c:cs) = undefined -- do command  
                      -- move name cs

defCom :: DefCom -> Salsa()
defCom ae = case ae of
  Def def -> definition def  
  Com cmd -> liftC (command cmd)


  
liftC :: SalsaCommand () -> Salsa ()
liftC (SalsaCommand pa) = Salsa $ \a -> 
  let res = (pa a)
  in res
  

  
  --
-- Define the function runProg
--

runProg :: Integer -> Program -> Animation
runProg = undefined




interp :: [DefCom] -> Salsa () 
interp [] = do return ()                        
interp (inst:prog) = do defCom inst
                        interp prog