module Semantics where

-- import Control.DeepSeq
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
-- import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
-- import qualified Data.Set as S
import qualified Debug.Trace

-- import System.IO

import qualified Absfoo as AST
import Printfoo(printTree)


trace :: Show a => String -> a -> b -> b
trace pref trac val = Debug.Trace.trace (pref ++ (show trac)) val

#define TRACE(x) (trace (__FILE__ ++ ":" ++ (show (__LINE__ :: Int)) ++ " ") (x) $ seq (x) $ return ())

data Err = Err String

showSemError :: Err -> String
showSemError (Err s) = "ERR: " ++ s

data GlobEnv = GlobEnv {
    types :: M.Map String Type,
    functions :: M.Map String Function,
    variants :: M.Map String String,  -- variant -> type
    nextTmp :: Int
}

newtype LocEnv = LocEnv {
    vars :: M.Map String ()
}

newtype RunEnv = RLocEnv {
    rvars :: M.Map String Value
}

data Loc = Loc String [AttrOrIdx] deriving (Eq, Ord, Show)

newTmpLoc :: TCM Loc
newTmpLoc = do
    env <- gets semEnv
    let n = nextTmp env
        env' = env{nextTmp=n+1}
    modify (\ss -> ss{semEnv=env'})
    return $ varLoc $ "tmp#" ++ (show n)

varLoc :: String -> Loc
varLoc vname = Loc vname []

setLoc :: Loc -> Value -> RunEnv -> RunEnv
setLoc (Loc vname fieldPath) newVal env = do
    env{rvars=M.alter f vname $ rvars env}
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
    case M.lookup vname $ rvars env of
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
    body :: QuadCode,
    isProc :: Bool
}

returnName :: String
returnName = "#"

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

-- type AArray = M.Map Int Value
-- type Object = M.Map String Value

type BinOpF = (Value -> Value -> Value)

type QLoc = Loc
type QOp = BinOpF
data QVal = QValConst Value
          | QValVar Loc
data Quad = Quad4 QLoc QVal QOp QVal
          | Quad2 QLoc QVal
          | QCall String [QVal] [QLoc]
          | QLoop QVal QuadCode
          | QBranch [(QVal, QuadCode)]
          | QReturn (Maybe QVal)
          | QAssert QVal
          | QPrint [QVal]
          | QInvalid  -- needed?

type QuadCode = [Quad]

-- newtype SSAQuadCode = SSAQuadCode QuadCode

data SemState = SemState {
    semContext :: Context,
    semErrors :: [Err],
    semEnv :: GlobEnv
}

type Context = [ContextLevel]

type ContextLevel = String

type TCM = State SemState

inContext :: ContextLevel -> TCM a -> TCM a
inContext ctx tcm = do
    octx <- gets semContext
    modify (\ss -> ss{semContext=ctx:octx})
    result <- tcm
    modify (\ss -> ss{semContext=octx})
    return result

mkCtx :: (Int, Int) -> String -> String
mkCtx (line, col) s = (show line) ++ ":" ++ (show col) ++ ":" ++ s

reportError :: String -> TCM ()
reportError errmsg = do
    st <- get
    let err = Err $ (show $ semContext st) ++ ": " ++ errmsg
    modify (\ss -> ss{semErrors=err:semErrors st})

runModule :: GlobEnv -> IO ()
runModule env = execQuad env $ QCall "main"  [] []

moduleSem :: AST.Module -> String -> Either [Err] GlobEnv
moduleSem (AST.Mod topDefs) _fileName = do
    ss <- return $ execState (do
                forM_ topDefs topDefSem
                hasMain <- gets $ (M.member "main").functions.semEnv
                when (not hasMain) $ do
                    reportError "no main() function"
            ) $ SemState [] [] initialEnv
    case semErrors ss of
        [] -> Right $ semEnv ss
        _ -> Left $ reverse $ semErrors ss
    where
        initialEnv = GlobEnv (M.fromList [("Int", Type)]) M.empty M.empty 0

topDefSem :: AST.TopDef -> TCM ()
topDefSem (AST.FunDef name genArgs argdefs mrettype (AST.Blk bodyStmts)) = do
    funDefSem False name genArgs argdefs mrettype bodyStmts
topDefSem (AST.ProcDef name genArgs argdefs mrettype (AST.Blk bodyStmts)) = do
    funDefSem True name genArgs argdefs mrettype bodyStmts
-- VariantTypeDef. TopDef ::= "type" Name GenArgsDef ":" "{" [VariantDef] "}" ;
-- SimpleTypeDef.  TopDef ::= "type" Name GenArgsDef ":" "{" [Field] "}" ;  -- just one variant
topDefSem topDef = reportError $ "topDefSem not yet implemented for " ++ (printTree topDef)

funDefSem :: Bool -> AST.Name -> AST.GenArgsDef -> [AST.ArgDef] -> AST.RetType -> [AST.Stmt] -> TCM ()
funDefSem proc (AST.Name (pos, name)) genArgDefs argdefs mrettype stmts = do
    inContext (mkCtx pos name) $ do
        when (genArgDefs /= AST.NoGenArgs) $
            reportError "genArgDefs /= AST.NoGenArgs not implemented yet"
        env <- gets semEnv
        if name `M.notMember` (functions env) then do
            let (argnames, outargMaybes) = unzip $ map (\ad -> case ad of
                        AST.ValArgDef _type (AST.Name (_, argName)) -> (argName, Nothing)
                        AST.VarArgDef _type (AST.Name (_, argName)) -> (argName, Just argName)
                    ) argdefs
                returns = case mrettype of
                    AST.JustType _type -> True
                    AST.NoType -> False
                (outargs, outlocs) = case (returns, catMaybes outargMaybes) of
                    (False, names) -> (names, map varLoc names)
                    (True, names) -> (returnName:names, varLoc returnName:map varLoc names)
            when (not proc && null outargs) $ do
                reportError "function with no output - should be proc"
            let lenv = LocEnv $ M.fromList $ map (\an -> (an, ())) argnames
            bodyQuads <- runReaderT (stmtsSem stmts) lenv
            let fun = (Function {
                argNames=argnames,
                argLocs=map varLoc argnames,
                outArgs=outargs,
                outLocs=outlocs,
                body=bodyQuads,
                isProc=proc
            })
            modify (\ss -> ss{semEnv=env{functions=M.insert name fun $ functions env}})
        else do
            reportError $ "function redefined: " ++ name

type LocTCM  = ReaderT LocEnv TCM

stmtsSem :: [AST.Stmt] -> LocTCM QuadCode
stmtsSem _ = do
    lift $ reportError "stmtsSem not implemented"
    return []

execQuad :: GlobEnv -> Quad -> IO ()
execQuad _ QInvalid = error "execQuad QInvalid"
execQuad _ _ = error "execQuadCode not implemented yet"

expAsLoc :: AST.Exp -> LocEnv -> Loc
expAsLoc (AST.ExpVoT [AST.Name (_, varName)]) _env = Loc varName []  -- TODO: check existence
expAsLoc destExp _env = error $ "Not lvalue or not yet handled as lvalue: " ++ (printTree destExp)

wrapBoolOp :: (Bool -> Bool -> Bool) -> (Value -> Value -> Value)
wrapBoolOp op v1 v2 = ValBool $ asBool v1 `op` asBool v2

wrapIntOp :: (Int -> Int -> Int) -> (Value -> Value -> Value)
wrapIntOp op v1 v2 = ValInt $ asInt v1 `op` asInt v2

wrapCmpOp :: (Int -> Int -> Bool) -> (String -> String -> Bool) -> (Char -> Char -> Bool) -> (Value -> Value -> Value)
wrapCmpOp op _ _ (ValInt v1) (ValInt v2) = ValBool $ v1 `op` v2
wrapCmpOp _ op _ (ValString v1) (ValString v2) = ValBool $ v1 `op` v2
wrapCmpOp _ _ op (ValChar v1) (ValChar v2) = ValBool $ v1 `op` v2
wrapCmpOp _ _ _ _ _ = error $ "wrapCmpOp: Incorrect or conflicting argument types"

cmpOpFun :: AST.CmpOp -> (Value -> Value -> Value)
cmpOpFun AST.EqOp = (ValBool.).(==)
cmpOpFun AST.NeOp = (ValBool.).(/=)
cmpOpFun AST.LtOp = wrapCmpOp (<) (<) (<)
cmpOpFun AST.GtOp = wrapCmpOp (>) (>) (>)
cmpOpFun AST.LeOp = wrapCmpOp (<=) (<=) (<=)
cmpOpFun AST.GeOp = wrapCmpOp (>=) (>=) (>=)
