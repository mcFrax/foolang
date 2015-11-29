{-# OPTIONS -XFlexibleInstances #-}
module Types where

import Control.Monad.State
import qualified Data.Map.Strict as M
import qualified Debug.Trace


trace :: Show a => String -> a -> b -> b
trace pref trac val = Debug.Trace.trace (pref ++ (show trac)) val

#define TRACE(x) (trace (__FILE__ ++ ":" ++ (show (__LINE__ :: Int)) ++ " ") (x) $ seq (x) $ return ())

data Err = Err String

showSemError :: Err -> String
showSemError (Err s) = "ERR: " ++ s

data Env = Env {
    types :: M.Map String Type,
    functions :: M.Map String Function,
    variants :: M.Map String String,  -- variant -> type
    nextTmp :: Int,
    vars :: Vars
}

type Vars = M.Map String ()

data RunEnv = RunEnv {
    staticEnv :: Env,
    varVals :: M.Map String Value
}

data Loc = Loc String [AttrOrIdx] deriving (Eq, Ord, Show)

newTmpLoc :: SM Loc
newTmpLoc = do
    env <- gets semEnv
    let n = nextTmp env
        env' = env{nextTmp=n+1}
    modify (\ss -> ss{semEnv=env'})
    return $ varLoc $ "tmp." ++ (show n)

varLoc :: String -> Loc
varLoc vname = Loc vname []

setLoc :: Loc -> Value -> RunEnv -> RunEnv
setLoc (Loc vname fieldPath) newVal env = do
    env{varVals=M.alter f vname $ varVals env}
    where
        f mval = do
            Just $ setField fieldPath newVal $ case mval of
                Nothing -> error $ "setLoc: Variable " ++ vname ++ " undefined"
                    -- it is actually an error only when fieldPath /= []
                (Just oldVal) -> oldVal

setLocs :: [(Loc, Value)] -> RunEnv -> RunEnv
setLocs assocs env = foldl (\e (l, v) -> setLoc l v e) env assocs

getLoc :: Loc -> RunEnv -> Value
getLoc (Loc vname fieldPath) env = do
    case M.lookup vname $ varVals env of
        Just val -> getField fieldPath val
        Nothing -> error $ "getLoc: Variable " ++ vname ++ " undefined"

setField :: [AttrOrIdx] -> Value -> Value -> Value
setField [] newVal _oldVal = newVal
setField _ _newVal _oldVal = error $ "setField not yet implemented for nonempty path"
-- setField (Idx idx:fieldPath') newVal oldVal = oldVal

getField :: [AttrOrIdx] -> Value -> Value
getField [] val = val
getField _ _val = error $ "getField not yet implemented for nonempty path"

data AttrOrIdx = Attr String | Idx Int deriving (Eq, Ord, Show)

data Function = Function {
    argNames :: [String],
    argLocs :: [QLoc],
    outArgs :: [String],  -- may contain returnName,  if returns non-void
    outLocs :: [QLoc],
    body :: QuadBlocks,
    isProc :: Bool,
    compile :: SM ()
}

returnName :: String
returnName = "."

data Type = Type

data Value = ValInt {asInt :: Int}
           | ValString {asString :: String}
           | ValBool {asBool :: Bool}
           | ValChar {asChar :: Char}
--            | ValArray {asArray :: AArray}
--            | ValObject {asObject :: Object}
    deriving (Eq, Show)

showVal :: Value -> String
showVal (ValInt i) = show i
showVal (ValBool b) = show b
showVal (ValChar c) = show c
showVal (ValString s) = show s
-- ...

isZero :: Value -> Bool
isZero (ValInt 0) = True
-- isZero (ValFloat 0.0) = True
isZero _ = False

-- type AArray = M.Map Int Value
-- type Object = M.Map String Value

type BinOpF = (Value -> Value -> Value)

instance Show (Value -> Value -> Value) where
    show _ = "<BinOpF>"

type QLoc = Loc
type QOp = BinOpF
data QVal = QValConst Value
          | QValVar Loc
          deriving Show
data Quad = Quad4 QLoc QVal QOp QVal
          | Quad2 QLoc QVal
          | QCall String [QVal] [Maybe QLoc]
          | QAssert QVal String
          | QPrint [QVal]
          deriving Show

data Jump = Jump BlockName | Branch QVal BlockName BlockName | Return

type BlockName = Int

type QuadCode = [Quad]

type QuadBlock = (QuadCode, Jump)

type QuadBlocks = M.Map BlockName QuadBlock


data SemState = SemState {
    semContext :: Context,
    semErrors :: [Err],
    semEnv :: Env,
    semNextFreeBlockName :: BlockName
}

type Context = [ContextLevel]

type ContextLevel = String

type SM = State SemState

type Run = StateT RunEnv IO