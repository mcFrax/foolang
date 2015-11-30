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

data Loc = Loc String [AttrOrIdx] deriving (Eq, Ord, Show)

varLoc :: String -> Loc
varLoc vname = Loc vname []

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
           | ValDouble Double
           | ValString {asString :: String}
           | ValBool {asBool :: Bool}
           | ValChar {asChar :: Char}
--            | ValArray {asArray :: AArray}
--            | ValObject {asObject :: Object}
    deriving (Eq, Show)

asDouble :: Value -> Double
asDouble (ValDouble v) = v
asDouble (ValInt v) = fromIntegral $ v
asDouble v = error $ "asDouble " ++ showVal v

showVal :: Value -> String
showVal (ValInt i) = show i
showVal (ValDouble f) = show f
showVal (ValBool b) = show b
showVal (ValChar c) = show c
showVal (ValString s) = show s
-- ...

isZero :: Value -> Bool
isZero (ValInt 0) = True
isZero (ValDouble 0.0) = True
isZero _ = False

-- type AArray = M.Map Int Value
-- type Object = M.Map String Value

type BinOpF = (Value -> Value -> Value)

type QLoc = Loc
data QOp = QAnd | QOr | QAdd | QSub | QMul | QDiv | QDivInt | QMod
         | QCmpEq | QCmpNe | QCmpLt | QCmpGt | QCmpLe | QCmpGe
            deriving (Show, Eq)
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

evalQOp :: QOp -> Value -> Value -> Value
evalQOp QAnd = wrapBoolOp (&&)
evalQOp QOr = wrapBoolOp (||)
evalQOp QAdd = wrapNumOp (+) (+)
evalQOp QSub = wrapNumOp (-) (-)
evalQOp QMul = wrapNumOp (*) (*)
evalQOp QDiv = wrapDoubleOp (/)
evalQOp QDivInt = wrapIntOp div
evalQOp QMod = wrapIntOp mod
evalQOp QCmpEq = (ValBool.).(==)
evalQOp QCmpNe = (ValBool.).(/=)
evalQOp QCmpLt = wrapCmpOp (<) (<) (<)
evalQOp QCmpGt = wrapCmpOp (>) (>) (>)
evalQOp QCmpLe = wrapCmpOp (<=) (<=) (<=)
evalQOp QCmpGe = wrapCmpOp (>=) (>=) (>=)

wrapBoolOp :: (Bool -> Bool -> Bool) -> (Value -> Value -> Value)
wrapBoolOp op v1 v2 = ValBool $ asBool v1 `op` asBool v2

wrapNumOp :: (Int -> Int -> Int) -> (Double -> Double -> Double) -> (Value -> Value -> Value)
wrapNumOp op _ (ValInt v1) (ValInt v2) = ValInt $ v1 `op` v2
wrapNumOp _ op v1 v2 = wrapDoubleOp op v1 v2

wrapIntOp :: (Int -> Int -> Int) -> (Value -> Value -> Value)
wrapIntOp op v1 v2 = ValInt $ asInt v1 `op` asInt v2

wrapDoubleOp :: (Double -> Double -> Double) -> (Value -> Value -> Value)
wrapDoubleOp op v1 v2 = ValDouble $ asDouble v1 `op` asDouble v2

wrapCmpOp :: (Int -> Int -> Bool) -> (String -> String -> Bool) -> (Char -> Char -> Bool) -> (Value -> Value -> Value)
wrapCmpOp op _ _ (ValInt v1) (ValInt v2) = ValBool $ v1 `op` v2
wrapCmpOp _ op _ (ValString v1) (ValString v2) = ValBool $ v1 `op` v2
wrapCmpOp _ _ op (ValChar v1) (ValChar v2) = ValBool $ v1 `op` v2
wrapCmpOp _ _ _ _ _ = error $ "wrapCmpOp: Incorrect or conflicting argument types"