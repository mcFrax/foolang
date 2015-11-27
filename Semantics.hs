{-# OPTIONS -XFlexibleInstances #-}
module Semantics where

import Control.Monad
import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Maybe
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
          | QLoop QVal QuadCode
          | QBranch [(QVal, QuadCode)]
          | QReturn
          | QAssert QVal
          | QPrint [QVal]
          | QInvalid  -- needed?
          deriving Show

type QuadCode = [Quad]

-- newtype SSAQuadCode = SSAQuadCode QuadCode

data SemState = SemState {
    semContext :: Context,
    semErrors :: [Err],
    semEnv :: Env
}

type Context = [ContextLevel]

type ContextLevel = String

type TCM = State SemState

type Run = StateT RunEnv IO

inContext :: ContextLevel -> TCM a -> TCM a
inContext ctx tcm = do
    octx <- gets semContext
    modify (\ss -> ss{semContext=ctx:octx})
    result <- tcm
    modify (\ss -> ss{semContext=octx})
    return result

mkCtx :: (Int, Int) -> String -> String
mkCtx (line, col) s = (show line) ++ ":" ++ (show col) ++ ":" ++ s

reportError :: String -> TCM a
reportError errmsg = do
    st <- get
    let err = Err $ (show $ semContext st) ++ ": " ++ errmsg
    modify (\ss -> ss{semErrors=err:semErrors st})
    return $ error "invalid [error reported]"

modifyEnv :: (Env -> Env) -> TCM ()
modifyEnv f = modify (\ss -> ss{semEnv=f $ semEnv ss})

modifyVars :: (Vars -> Vars) -> TCM ()
modifyVars f = modifyEnv (\env -> env{vars=f $ vars env})

runModule :: Env -> IO ()
runModule env = do
    _ <- execStateT (execQuad $ QCall "main" [] []) $ RunEnv env M.empty
    return ()

moduleSem :: AST.Module -> String -> Either [Err] Env
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
        initialEnv = Env (M.fromList [("Int", Type)]) M.empty M.empty 0 M.empty

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
            modifyVars $ const $ M.fromList $ map (\an -> (an, ())) argnames
            bodyQuads <- stmtsSem stmts
            modifyVars $ const $ vars env
            let fun = (Function {
                argNames=argnames,
                argLocs=map varLoc argnames,
                outArgs=outargs,
                outLocs=outlocs,
                body=bodyQuads,
                isProc=proc
            })
            modifyEnv (\env' -> env'{functions=M.insert name fun $ functions env'})
        else do
            reportError $ "function redefined: " ++ name

stmtsSem :: [AST.Stmt] -> TCM QuadCode
stmtsSem stmts = do
    stmts' <- fixIfs stmts
    liftM concat $ mapM stmtSem stmts'
    where
        fixIfs :: [AST.Stmt] -> TCM [AST.Stmt]
        fixIfs [] = return []
        fixIfs ((AST.StmtIf cond block):stmts') = do
            let (elifs, melse, stmts'') = takeElifs stmts'
            stmts''' <- fixIfs stmts''
            elseBlk <- case melse of
                    Just elseBlk -> return elseBlk
                    Nothing -> return $ AST.Blk []
            return $ (AST.StmtIfElse cond block elifs elseBlk):stmts'''
        fixIfs (AST.StmtElif{}:_) = error $ "Unexpected elif clause"
        fixIfs (AST.StmtElse{}:_) = error $ "Unexpected else clause"
        fixIfs (stmt:ss) = do
            rest <- fixIfs ss
            return $ stmt:rest
        takeElifs :: [AST.Stmt] -> ([AST.ElifClause], Maybe AST.Block, [AST.Stmt])
        takeElifs ((AST.StmtElif cond block):stmts') = do
            let (elifs, melse, stmts'') = takeElifs stmts'
            ((AST.Elif cond block):elifs, melse, stmts'')
        takeElifs ((AST.StmtElse block):stmts') = do
            ([], Just block, stmts')
        takeElifs stmts' = ([], Nothing, stmts')

stmtSem :: AST.Stmt -> TCM QuadCode
stmtSem s = do
    inContext (stmtCtx s) $ do
        stmtSem' s
    where
-- StmtLAssign.     Stmt ::= Exp "<-" Exp ;
-- StmtRAssign.     Stmt ::= Exp "->" Exp ;
-- StmtPass.        Stmt ::= "pass" ;
-- StmtAssert.      Stmt ::= "assert" Exp ;
-- StmtStatAssert.  Stmt ::= "static" "assert" Exp ;
-- StmtPrint.       Stmt ::= "!" [Exp] ;
-- StmtExp.         Stmt ::= Exp ;
-- StmtReturn.      Stmt ::= "return" ;
-- StmtReturnValue. Stmt ::= "return" Exp ;
-- StmtIf.          Stmt ::= "if" Exp Block ;
-- StmtElif.        Stmt ::= "elif" Exp Block ;
-- StmtElse.        Stmt ::= "else" Block ;
-- StmtWhile.       Stmt ::= "while" Exp  Block ;
-- StmtForIn.       Stmt ::= "for" Name "in" Exp Block ;
-- StmtForVarIn.    Stmt ::= "for" "->" Name "in" Exp Block ;
-- StmtBreak.       Stmt ::= "break" ;
-- StmtContinue.    Stmt ::= "continue" ;
-- StmtCase.        Stmt ::= "case" [Exp] ":" "{" [CasePattern] "}";
        stmtSem' AST.StmtPass = return []
        stmtSem' (AST.StmtPrint exprs) = do
            (exprsQuads, exprVals) <- liftM unzip $ mapM pureExprSem exprs
            return $ concat $ exprsQuads ++ [[QPrint exprVals]]
        stmtSem' _ = do
            reportError $ "stmtSem: Statement not yet implemented"

stmtCtx :: AST.Stmt -> ContextLevel
stmtCtx = printTree

pureExprSem :: AST.Exp -> TCM (QuadCode, QVal)
pureExprSem e =
    inContext (exprCtx e) $ do
        pureExprSem' e
    where
        pureExprSem' (AST.ExpInt i) = return ([], QValConst $ ValInt $ fromInteger i)
        pureExprSem' (AST.ExpStr s) = return ([], QValConst $ ValString $ s)
        pureExprSem' (AST.ExpAnd lExp rExp) = pureBinOpSem (wrapBoolOp (&&)) lExp rExp
        pureExprSem' (AST.ExpOr lExp rExp) = pureBinOpSem (wrapBoolOp (||)) lExp rExp
        pureExprSem' (AST.ExpCmp lExp cmpOp rExp) = pureBinOpSem (cmpOpFun cmpOp) lExp rExp
        pureExprSem' (AST.ExpAdd lExp rExp) = pureBinOpSem (wrapIntOp (+)) lExp rExp
        pureExprSem' (AST.ExpSub lExp rExp) = pureBinOpSem (wrapIntOp (-)) lExp rExp
        pureExprSem' (AST.ExpMul lExp rExp) = pureBinOpSem (wrapIntOp (*)) lExp rExp
--         pureExprSem' (AST.ExpDiv lExp rExp) = pureBinOpSem (/) lExp rExp
        pureExprSem' (AST.ExpDivInt lExp rExp) = pureBinOpSem (wrapIntOp div) lExp rExp
        pureExprSem' (AST.ExpMod lExp rExp) = pureBinOpSem (wrapIntOp mod) lExp rExp
        pureExprSem' _ = do
            reportError $ "stmtSem: Statement not yet implemented"

pureBinOpSem :: BinOpF -> AST.Exp -> AST.Exp -> TCM (QuadCode, QVal)
pureBinOpSem op lExp rExp = do
    (lCode, lQVal) <- pureExprSem lExp
    (rCode, rQVal) <- pureExprSem rExp
    case (lCode, lQVal, rCode, rQVal) of
         -- TODO: special handling for div (error on 0) and possibly other ops (like overflow)
         ([], QValConst lConst, [], QValConst rConst) -> do
            return ([], QValConst $ lConst `op` rConst)
         _ -> do
            destLoc <- newTmpLoc
            return (lCode ++ rCode ++ [Quad4 destLoc lQVal op rQVal], QValVar destLoc)

-- runNonPureBinOp :: BinOpF -> AST.Exp -> AST.Exp -> AST.Exp -> Env -> IO Env
-- runNonPureBinOp op wholeExpr lexp rexp env = do
--     (locExp, lexp', rexp') <- case (lexp, rexp) of
--             (AST.ExpArrow _, AST.ExpArrow _) -> error $ "Multiple arrows in " ++ (printTree wholeExpr)
--             (AST.ExpArrow lexp', _) -> return (lexp', lexp', rexp)
--             (_, AST.ExpArrow rexp') -> return (rexp', lexp, rexp')
--             _ -> error $ "runNonPureBinOp: non-pure expression expected, but no top-level arrows found " ++ (printTree wholeExpr)
--     let destLoc = expAsLoc locExp env
--     val <- runPureBinOp op lexp' rexp' env
--     return $ setLoc destLoc val env
--
-- runPureBinOp :: BinOpF -> AST.Exp -> AST.Exp -> Env -> IO Value
-- runPureBinOp op lexp rexp env = do
--     lval <- runPureExpr lexp env
--     rval <- runPureExpr rexp env
--     return $ lval `op` rval

exprCtx :: AST.Exp -> ContextLevel
exprCtx = printTree

execQuad :: Quad -> Run ()
execQuad QInvalid = error "execQuad QInvalid"
execQuad (QPrint qvals) = do
    forM_ qvals $ \qval -> do
        s <- liftM ((++ " ") . showVal) $ execQVal qval
        liftIO $ putStr s
    liftIO $ putStrLn ""
execQuad (QCall fname args outs) = do
    re <- get
    let f = (functions $ staticEnv re) M.! fname
    argVals <- mapM execQVal args
    let argVars = M.fromList $ argNames f `zip` argVals
    put re{varVals=argVars}
--     forM_ (body f) execQuad
    forM_ (body f) $ \q -> do
        liftIO $ print q
        execQuad q
    inEnv <- get
    let re' = foldl (\re'' (inLoc, outMLoc) -> do
                case outMLoc of
                    Just outLoc -> do
                        let val = getLoc inLoc inEnv
                        setLoc outLoc val re''
                    Nothing -> re''
            ) re (outLocs f `zip` outs)
    put re'
execQuad (Quad4 destLoc lQVal qop rQVal) = do
    resultVal <- liftM2 qop (execQVal lQVal) (execQVal rQVal)
    modify $ setLoc destLoc resultVal
execQuad (Quad2 destLoc qVal) = do
    val <- execQVal qVal
    modify $ setLoc destLoc val
execQuad quad = error $ "execQuad not implemented yet for " ++ show quad

execQVal :: QVal -> Run Value
execQVal (QValConst val) = return val
execQVal (QValVar loc) = do
    liftM (getLoc loc) $ get

expAsLoc :: AST.Exp -> TCM Loc
expAsLoc (AST.ExpVoT [AST.Name (_, varName)]) = return $ Loc varName []
expAsLoc destExp = error $ "Not lvalue or not yet handled as lvalue: " ++ (printTree destExp)

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
