{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Semantic.Checker (semanticCheck) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import SimpleImperativeLanguage.Abs

-- Available types in the language.
data Type = TInt | TBool deriving (Eq, Show)

-- Symbol table to track variable declarations and their types.
type SymbolTable = Map.Map Ident Type
emptySymbolTable :: SymbolTable
emptySymbolTable = Map.empty

type SymbolSet = Set.Set Ident
emptySymbolSet :: SymbolSet
emptySymbolSet = Set.empty


-- -- checkComm takes a Command a SymbolTable and returns either an error message or the new SymbolTable.
-- checkComm :: Comm -> SymbolTable -> Either String SymbolTable
-- checkComm Skip st = Right st
-- checkComm (LetInt x e) st = do
--   -- Check the expression
--   st' <- checkIntExp e st
--   -- Check the variable is not already declared
--   if Map.member x st'
--     then Left $ variableAlreadyDeclaredError x
--     else return $ Map.insert x TInt st'
-- checkComm (LetBool x e) st = do
--   -- Check the expression
--   st' <- checkBoolExp e st
--   -- Check the variable is not already declared
--   if Map.member x st
--     then Left $ variableAlreadyDeclaredError x
--     else return $ Map.insert x TBool st'
-- checkComm (AssInt x e) st = do
--   -- Check the expression
--   st' <- checkIntExp e st
--   -- Check the variable is declared
--   case Map.lookup x st' of
--     Just TInt -> return st'
--     Just nt -> Left $ typeMismatchError x TInt nt
--     Nothing -> Left $ variableNotDeclaredError x
-- checkComm (AssBool x e) st = do
--   -- Check the expression
--   st' <- checkBoolExp e st
--   -- Check the variable is declared
--   case Map.lookup x st' of
--     Just TBool -> return st'
--     Just nt -> Left $ typeMismatchError x TBool nt
--     Nothing -> Left $ variableNotDeclaredError x
-- checkComm (Seq c1 c2) st = do
--   st' <- checkComm c1 st
--   checkComm c2 st'
-- checkComm (IfThenElse b c1 c2) st = do
--   _ <- checkBoolExp b st
--   -- Variables declared in the branches are not visible in the other branch
--   _ <- checkComm c1 st
--   _ <- checkComm c2 st
--   -- Variables declared in the branches are not visible after the if-then-else  
--   return st
-- checkComm (While b c) st = do
--   _ <- checkBoolExp b st
--   _ <- checkComm c st
--   -- Variables declared in the loop are not visible after the loop
--   return st

-- -- checkIntExp takes an expression, a type and a SymbolTable and returns either an error message or a SymbolTable.
-- checkIntExp :: IntExp -> SymbolTable -> Either String SymbolTable
-- checkIntExp (Exp _) st = Right st
-- checkIntExp (Var x) st = case Map.lookup x st of
--   Just TInt -> Right st
--   Just nt -> Left $ typeMismatchError x TInt nt
--   Nothing -> Left $ variableNotDeclaredError x
-- checkIntExp (UMinus e) st = do
--   _ <- checkIntExp e st
--   return st
-- checkIntExp binaryIntOp st = do
--   (e1, e2) <- unwarpBinaryIntOp binaryIntOp
--   _ <- checkIntExp e1 st
--   _ <- checkIntExp e2 st
--   return st

-- unwarpBinaryIntOp :: IntExp -> Either String (IntExp, IntExp)
-- unwarpBinaryIntOp (Plus e1 e2) = Right(e1, e2)
-- unwarpBinaryIntOp (Minus e1 e2) = Right(e1, e2)
-- unwarpBinaryIntOp (Times e1 e2) = Right(e1, e2)
-- unwarpBinaryIntOp (Div e1 e2) = Right(e1, e2)
-- unwarpBinaryIntOp _ = Left "Not a binary integer operation" -- Todo: Change this to a more descriptive error message

-- -- checkBoolExp takes a boolean expression, a SymbolTable and returns either an error message or a SymbolTable.
-- checkBoolExp :: BoolExp -> SymbolTable -> Either String SymbolTable
-- checkBoolExp BTrue st = Right st
-- checkBoolExp BFalse st = Right st
-- checkBoolExp (Eq e1 e2) st = do
--   _ <- checkIntExp e1 st
--   _ <- checkIntExp e2 st
--   return st
-- checkBoolExp (NEq e1 e2) st = do
--   _ <- checkIntExp e1 st
--   _ <- checkIntExp e2 st
--   return st
-- checkBoolExp (Lt e1 e2) st = do
--   _ <- checkIntExp e1 st
--   _ <- checkIntExp e2 st
--   return st
-- checkBoolExp (Gt e1 e2) st = do
--   _ <- checkIntExp e1 st
--   _ <- checkIntExp e2 st
--   return st
-- checkBoolExp (And e1 e2) st = do
--   _ <- checkBoolExp e1 st
--   _ <- checkBoolExp e2 st
--   return st
-- checkBoolExp (Or e1 e2) st = do
--   _ <- checkBoolExp e1 st
--   _ <- checkBoolExp e2 st
--   return st
-- checkBoolExp (Not e) st = do
--   _ <- checkBoolExp e st
--   return st
-- checkBoolExp (VarBool x) st = case Map.lookup x st of
--   Just TBool -> Right st
--   Just nt -> Left $ typeMismatchError x TBool nt -- nt will always be TInt.
--   Nothing -> Left $ variableNotDeclaredError x

-- Shortcuts for error messages
-- typeMismatchError is a helper function to generate error messages for type mismatches.
typeMismatchError :: Ident -> Type -> Type -> String
typeMismatchError var expected actual = "Variable " ++ show var ++ " expected type " ++ show expected ++ " but got " ++ show actual

-- variableNotDeclaredError is a helper function to generate error messages for undeclared variables.
variableNotDeclaredError :: Ident -> String
variableNotDeclaredError var = "Variable " ++ show var ++ " not declared"

-- variableAlreadyDeclaredError is a helper function to generate error messages for redeclared variables.
variableAlreadyDeclaredError :: Ident -> String
variableAlreadyDeclaredError var = "Variable " ++ show var ++ " already declared"

-- skipOutsideLoopError is a helper function to generate error messages for the use of skip outside loops.
skipOutsideLoopError :: String
skipOutsideLoopError = "Use of `skip` outside loop"


-----------------------------------------------------------
-- Define a sum type to represent all state types
data CheckerState
    = StateUndeclaredVariables UndeclaredVariables
    | StateRedeclaredVariables RedeclaredVariables
    | StateTypeMismatch TypeMismatch
    | StateSkipOutsideLoop SkipOutsideLoop

-- Existential wrapper
data AnyChecker = forall s. Checker s => AnyChecker s

initStates :: [AnyChecker]
initStates = [AnyChecker newStateUndeclaredVariables, AnyChecker newStateRedeclaredVariables, AnyChecker newStateTypeMismatch, AnyChecker newStateSkipOutsideLoop]

-- General checker function for Comm
generalCheckerComm :: Comm -> [AnyChecker] -> [State CheckerState]
generalCheckerComm comm = map (\(AnyChecker s) -> checkerComm comm s)

extractErrors :: State CheckerState -> Maybe [String]
extractErrors (_, errors) = errors

concatErrors :: Maybe [String] -> Maybe [String] -> Maybe [String]
concatErrors Nothing Nothing = Nothing
concatErrors (Just errors) Nothing = Just errors
concatErrors Nothing (Just errors) = Just errors
concatErrors (Just errors1) (Just errors2) = Just (errors1 ++ errors2)

-- semanticCheck
semanticCheck :: Comm -> Maybe [String]
semanticCheck comm = foldr
    (concatErrors . extractErrors)
    Nothing
    (generalCheckerComm comm initStates)
-----------------------------------------------------------
-- Checker typeclass

-- Define the State type as a tuple of the type t and a list of errors.
type State t = (t, Maybe [String]) -- The list of string is a list of errors. If it is Nothing, there are no errors.
addError :: String -> State t -> State t
addError err (t, Just errors) = (t, Just (err : errors))
addError err (t, Nothing)     = (t, Just [err])

-- Checker is a typeclass that defines the methods that a checker must implement.
class Checker t where
    checkerComm    :: Comm    -> State t -> State t
    checkerIntExp  :: IntExp  -> State t -> State t
    checkerBoolExp :: BoolExp -> State t -> State t
    inLoop         :: State t -> State t
    outLoop        :: State t -> State t -> State t
    inConditional  :: State t -> State t
    outConditional :: State t -> State t -> State t

-----------------------------------------------------------
-- Undeclared variables:
-- - Add var in LetInt and LetBool
-- - Check in Var and VarBool
-- - Remove when exiting scope ( IfThenElse and While )
newtype UndeclaredVariables = UndeclaredVariables SymbolSet
memberU :: Ident -> UndeclaredVariables -> Bool
memberU key (UndeclaredVariables symbolSet) = Set.member key symbolSet
insertU :: Ident -> UndeclaredVariables -> UndeclaredVariables
insertU key (UndeclaredVariables symbolSet) = UndeclaredVariables (Set.insert key symbolSet)
newUndeclaredVariables :: UndeclaredVariables
newUndeclaredVariables = UndeclaredVariables emptySymbolSet
newStateUndeclaredVariables :: State UndeclaredVariables
newStateUndeclaredVariables = (newUndeclaredVariables, Nothing)

instance Checker (State UndeclaredVariables) where
    checkerComm (LetInt x _)  (undeclaredVariables, errors) = insertU x undeclaredVariables
    checkerComm (LetBool x _) (undeclaredVariables, errors) = (insertU x undeclaredVariables, errors)
    checkerComm (AssInt _ _)  st = st
    checkerComm (AssBool _ _) st = st
    checkerComm _             st = st
    checkerIntExp (Var x) st =
      let (undeclaredVariables, _) = st in
      if memberU x undeclaredVariables
        then st
        else addError (variableNotDeclaredError x) st
    checkerIntExp _       (st, errors) = (st, errors)
    checkerBoolExp (VarBool x) st = let (undeclaredVariables, _) = st in
      if memberU x undeclaredVariables
        then st
        else addError (variableNotDeclaredError x) st
    checkerBoolExp _ st = st
    inLoop st           = st
    outLoop st _        = st
    inConditional st    = st
    outConditional st _ = st

-----------------------------------------------------------
-- Redeclared variables:
-- - Add var in LetInt and LetBool
-- - Check in LetInt and LetBool
-- - Remove when exiting scope ( IfThenElse and While )
newtype RedeclaredVariables = RedeclaredVariables SymbolSet
memberR :: Ident -> RedeclaredVariables -> Bool
memberR key (RedeclaredVariables symbolSet) = Set.member key symbolSet
insertR :: Ident -> RedeclaredVariables -> RedeclaredVariables
insertR key (RedeclaredVariables symbolSet) = RedeclaredVariables (Set.insert key symbolSet)
newRedeclaredVariables :: RedeclaredVariables
newRedeclaredVariables = RedeclaredVariables emptySymbolSet
newStateRedeclaredVariables :: State RedeclaredVariables
newStateRedeclaredVariables = (newRedeclaredVariables, Nothing)

instance Checker (State RedeclaredVariables) where
    checkerComm (LetInt x _)  st = let (redeclaredVariables, errors) = st in
      if memberR x redeclaredVariables
        then addError (variableAlreadyDeclaredError x) st
        else (insertR x redeclaredVariables, errors)
    checkerComm (LetBool x _) st = let (redeclaredVariables, errors) = st in
      if memberR x redeclaredVariables
        then addError (variableAlreadyDeclaredError x) st
        else (insertR x redeclaredVariables, errors)
    checkerComm _             st = st
    checkerIntExp _           st = st
    checkerBoolExp _          st = st
    inLoop st                 = st
    outLoop st _              = st
    inConditional st          = st
    outConditional st _       = st

-----------------------------------------------------------
-- Type mismatch:
-- - Add var in LetInt and LetBool
-- - Check in AssInt and AssBool
-- - Remove when exiting scope ( IfThenElse and While )
newtype TypeMismatch = TypeMismatch SymbolTable
lookupT :: Ident -> TypeMismatch -> Maybe Type
lookupT key (TypeMismatch table) = Map.lookup key table
insertT :: Ident -> Type -> TypeMismatch -> TypeMismatch
insertT key value (TypeMismatch table) = TypeMismatch (Map.insert key value table)
newTypeMismatch :: TypeMismatch
newTypeMismatch = TypeMismatch emptySymbolTable
newStateTypeMismatch :: State TypeMismatch
newStateTypeMismatch = (newTypeMismatch, Nothing)

instance Checker (State TypeMismatch) where
    checkerComm (LetInt x _)  (typeMismatch, errors) = (insertT x TInt typeMismatch, errors)
    checkerComm (LetBool x _) (typeMismatch, errors) = (insertT x TBool typeMismatch, errors)
    checkerComm (AssInt x _)  (typeMismatch, errors) = case lookupT x typeMismatch of
        Just TInt -> (typeMismatch, errors)
        Nothing   -> (typeMismatch, errors)
        Just nt   -> addError (typeMismatchError x TInt nt) (typeMismatch, errors)
    checkerComm (AssBool x _) (typeMismatch, errors) = case lookupT x typeMismatch of
        Just TBool -> (typeMismatch, errors)
        Nothing    -> (typeMismatch, errors)
        Just nt    -> addError (typeMismatchError x TBool nt) (typeMismatch, errors)
    checkerComm _             st = st
    checkerIntExp _           st = st
    checkerBoolExp _          st = st
    inLoop st                 = st
    outLoop st _              = st
    inConditional st          = st
    outConditional st _       = st

------------------------------------------------
-- Check skip INSIDE while loop
-- - Add in For loop
-- - Check when skip is used
-- - Remove when exiting scope ( IfThenElse and While )
newtype SkipOutsideLoop = SkipOutsideLoop Int
addOne :: SkipOutsideLoop -> SkipOutsideLoop
addOne (SkipOutsideLoop counter) = SkipOutsideLoop (counter + 1)
isZero :: SkipOutsideLoop -> Bool
isZero (SkipOutsideLoop counter) = counter == 0
newSkipOutsideLoop :: SkipOutsideLoop
newSkipOutsideLoop = SkipOutsideLoop 0
newStateSkipOutsideLoop :: State SkipOutsideLoop
newStateSkipOutsideLoop = (newSkipOutsideLoop, Nothing)

instance Checker (State SkipOutsideLoop) where
    checkerComm (While _ _) (skipOutsideLoop, errors) = (addOne skipOutsideLoop, errors)
    checkerComm Skip        (skipOutsideLoop, errors) = if isZero skipOutsideLoop
      then addError skipOutsideLoopError (skipOutsideLoop, errors)
      else (skipOutsideLoop, errors)
    checkerComm _           st = st
    checkerIntExp _         st = st
    checkerBoolExp _        st = st
    inLoop st               = st
    outLoop st _            = st
    inConditional st        = st
    outConditional st _     = st

-- State t = (t , Maybe [String])
-- Checker t:: Comm -> State t-> State t
-- checkerUndeclared   :: Comm -> State SymbolTable -> State SymbolTable
-- checkerTypeMismatch :: Comm -> State SymbolTable -> State SymbolTable
-- checkerRedeclare    :: Comm -> State SymbolTable -> State SymbolTable
-- checkerSkipInLoop   :: Comm -> State Int -> State Int

-- checkerComm    :: Comm    -> [Checker] -> [State] -> [State]
-- checkerIntExp  :: IntExp  -> [Checker] -> [State] -> [State]
-- checkerBoolExp :: BoolExp -> [Checker] -> [State] -> [State]
-- checkerComm (While b c) cs ss = do
--  ss' fmap inLoop cs ss
--  ss'' <- checkerIntExp b cs ss'
--  ss''' <- checkerComm c cs ss''
--  return fmap outLoop cs ss
     

-- inWhile :: Checker -> State -> State
--               c       prev  -> inter
-- outWhile ::  Checker -> State -> State -> State
--               c         prev     post  ->  res

-- inIfThenElse :: Checker -> State -> State
--                    c       prev      inter

-- outIfThenElse :: Checker -> State -> State
--                     c       prev      post

-- inLetInt :: Checker -> State -> State
