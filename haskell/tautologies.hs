import Data.List
import Data.Maybe (fromMaybe)

data Prop = 
    Const Bool
  | Var Char
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  deriving Show

findAllVars :: Prop -> [Char]
findAllVars (Const _) = []
findAllVars (Var c) = [c]
findAllVars (Not p) = findAllVars p
findAllVars (And p pn) = findAllVars p ++ findAllVars pn
findAllVars (Or p pn) = findAllVars p ++ findAllVars pn

evalProp :: Prop -> [(Char, Bool)] -> Bool
evalProp (Const b) _ = b
evalProp (Var c) asgnt = fromMaybe False (lookup c asgnt)
evalProp (Not p) l = not (evalProp p l)
evalProp (And p pn) l = evalProp p l && evalProp pn l
evalProp (Or p pn) l = evalProp p l || evalProp pn l

allAssignements :: [Char] -> [[(Char, Bool)]]
allAssignements vars
    | null vars = [[]]
    | otherwise = [ (head vars, b):rest| b <- [True, False], rest <- allAssignements (tail vars)]

isTaut :: Prop -> Bool
isTaut p = nub [ evalProp p a | a <- allAssignements (findAllVars p) ] == [True]

test1 = isTaut (Var 'a')
test2 = isTaut (Or (And (Not (Var 'a')) (Not (Var 'b')))
             (Or (Var 'a') (Var 'b')) )