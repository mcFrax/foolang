module Semantics where

-- import Control.DeepSeq
import Control.Monad
-- import Control.Monad.Reader.Class
-- import Control.Monad.State.Class
-- import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.List
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

data Env = Env {
    types :: M.Map String Type,
    functions :: M.Map String Function,
    variants :: M.Map String String,  -- variant -> type
    vars :: M.Map String Value
}

data Loc = Loc String [AttrOrIdx] deriving (Eq, Ord, Show)

varLoc :: String -> Loc
varLoc vname = Loc vname []

setLoc :: Loc -> Value -> Env -> Env
setLoc (Loc vname fieldPath) newVal env = do
    env{vars=M.alter f vname $ vars env}
    where
        f mval = do
            Just $ setField fieldPath newVal $ case mval of
                Nothing -> error $ "Variable " ++ vname ++ " undefined"
                    -- it is actually an error only when fieldPath /= []
                (Just oldVal) -> oldVal

setLocs :: [(Loc, Value)] -> Env -> Env
setLocs assocs env = foldl (\e (l, v) -> setLoc l v e) env assocs

getLoc :: Loc -> Env -> Value
getLoc (Loc vname fieldPath) env = do
    case M.lookup vname $ vars env of
        Just val -> getField fieldPath val
        Nothing -> error $ "Variable " ++ vname ++ " undefined"

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
    argLocs :: [Loc],
    returns :: Bool,
    outArgs :: [String],
    outArgLocs :: [Loc],
    body :: [AST.Stmt]
}

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

moduleSem :: AST.Module -> String -> Either Err (IO ())
moduleSem (AST.Mod topDefs) _fileName = do
    let env = foldl topDefSem (Env (M.fromList [("Int", Type)]) M.empty M.empty M.empty) topDefs
    return $ do
        _ <- runFunction (functions env M.! "main") env [] [] Nothing
        return ()

topDefSem :: Env -> AST.TopDef -> Env
topDefSem env (AST.FunDef (AST.Name (_, name)) AST.NoGenArgs argdefs mrettype (AST.Blk fbody)) = do
    let (argnames, outargMaybes) = unzip $ map (\ad -> case ad of
                AST.ValArgDef _type (AST.Name (_, argName)) -> (argName, Nothing)
                AST.VarArgDef _type (AST.Name (_, argName)) -> (argName, Just argName)
            ) argdefs
        freturns = case mrettype of
            AST.JustType _type -> True
            AST.NoType -> False
    env{functions=M.insert name (Function {
            argNames=argnames,
            argLocs=map varLoc argnames,
            returns=freturns,
            outArgs=catMaybes outargMaybes,
            outArgLocs=map varLoc $ catMaybes outargMaybes,
            body=fbody
        }) (functions env)}
-- VariantTypeDef. TopDef ::= "type" Name GenArgsDef ":" "{" [VariantDef] "}" ;
-- SimpleTypeDef.  TopDef ::= "type" Name GenArgsDef ":" "{" [Field] "}" ;  -- just one variant
topDefSem _ topDef = error $ "topDefSem not yet implemented for " ++ (printTree topDef)

runFunction :: Function -> Env -> [Value] -> [Maybe Loc] -> Maybe Loc -> IO Env
runFunction f oenv args argouts mout = do
    (ienv, mret) <- runStmts (body f) $ setLocs (argLocs f `zip` args) $ oenv{vars=M.empty}
    let updates = case (outArgLocs f, returns f) of
            ([], False) -> []  -- no value returned
            ([], True) -> do  -- value returned, but no out args
                case (mret, mout, catMaybes argouts) of
                    (Nothing, Nothing, []) -> []
                    (Just _, Nothing, []) -> error "Result ignored!"
                    (Just result, Just loc, []) -> [(loc, result)]
                    (Just result, Nothing, [loc]) -> [(loc, result)]
                    (Just _, _, _) -> error "Many places for one result? Fuck you!"
                    (Nothing, _, _) -> error "No value returned althought expected"
            ([oan], False) -> do  -- exactly one out arg in this function
                let retval = getLoc oan ienv
                case (mret, mout, catMaybes argouts) of
                    (Nothing, Nothing, []) -> error "Result ignored!"
                    (Nothing, Nothing, [loc]) -> [(loc, retval)]
                    (Nothing, Just loc, []) -> [(loc, retval)]
                    (Nothing, _, _) -> error "Many places for one result? Fuck you!"
                    (Just _, _, _) -> error "Unexpected return"
            (_oans, _) -> do
                let retupdate = case (returns f, mret, mout) of
                        (True, Just result, Just loc) -> [(loc, result)]
                        (False, Nothing, Nothing) -> []
                        (_, _, _) -> error "Screw you! That doesn't make sense!"
                    unpack (Just oenvName, value) = Just (oenvName, value)
                    unpack (Nothing, _) = Nothing
                let mappedOuts = mapMaybe (\(an, ao) -> ao >> Just an) (zip (argNames f) argouts)
                when (mappedOuts /= outArgs f) $ error $ "Args with arrows don't match with signature"
                retupdate ++ mapMaybe unpack (argouts `zip` (map (flip getLoc ienv) $ (argLocs f)))
    return $ setLocs updates oenv

runStmts :: [AST.Stmt] -> Env -> IO (Env, Maybe Value)
runStmts stmts env = do
    stmts' <- fixIfs stmts
    (env', mmret) <- foldM runStmtOrReturn (env, Nothing) stmts'
    let mret = (mmret >>= id) -- Nothing->Nothing|Just x->x
    return (env', mret)
    where
        fixIfs [] = []
        fixIfs (AST.StmtIf{}
        fixIfs (AST.StmtElif{}:_) = error $ "Unexpected elif clause"
        fixIfs (AST.StmtElse{}:_) = error $ "Unexpected else clause"
        isElif (AST.StmtElif{}) = True
        isElif _ = False
        runStmtOrReturn (env'', Just retval) _ = return (env'', Just retval)
        runStmtOrReturn (env'', Nothing) stmt = runStmt stmt env''

runStmt :: AST.Stmt -> Env -> IO (Env, Maybe (Maybe Value))
runStmt (AST.StmtPass) env = return (env, Nothing)
runStmt (AST.StmtReturn) env = return (env, Just Nothing)
runStmt (AST.StmtReturnValue expr) env = do
    val <- runPureExpr expr env
    return (env, Just $ Just val)
runStmt (AST.StmtPrint exprs) env = do
    forM_ exprs $ \expr -> do
        val <- runPureExpr expr env
        putStr $ showVal val ++ " "
    putStrLn ""
    return (env, Nothing)
runStmt (AST.StmtAssert expr) env = do
    val <- runPureExpr expr env
    case val of
         ValBool True -> return (env, Nothing)
         ValBool False -> error $ "Assertion failed: " ++ (printTree expr)
         _ -> error $ "Assertion condition is a bool: " ++ (printTree expr)

-- runStmt (AST.StmtLet locExp expr) env = do
--     let destLoc = expAsLoc locExp env
--     val <- runPureExpr expr env
--     return (M.insert destLoc val env, Nothing)
runStmt (AST.StmtLAssign locExp expr) env = do
    let destLoc = expAsLoc locExp env
    env' <- runNonPureExpr (Just destLoc) expr env
    return (env', Nothing)
runStmt (AST.StmtRAssign expr locExp) env = do
    runStmt (AST.StmtLAssign locExp expr) env
runStmt (AST.StmtExp expr) env = do
    env' <- runNonPureExpr Nothing expr env
    return (env', Nothing)
runStmt stmt _env = error $ "runStmt not yet implemented for " ++ (printTree stmt)
-- ###StmtLet.          Stmt ::= "let" Exp60 "=" Exp ;
-- StmtTypedLet.     Stmt ::= "let" Exp71 Name "=" Exp ;
-- ###StmtAssign.       Stmt ::= Exp "<-" Exp ;
-- ###StmtPass.         Stmt ::= "pass" ;
-- ###StmtAssert.       Stmt ::= "assert" Exp ;
-- StmtStatAssert.   Stmt ::= "static" "assert" Exp ;
-- ###StmtPrint.        Stmt ::= "!" Exp ;
-- ###StmtExp.          Stmt ::= Exp ;
-- ###StmtReturn.      Stmt ::= "return" ;
-- ###StmtReturnValue. Stmt ::= "return" Exp ;
-- StmtIf.          Stmt ::= "if" Exp Block ;
-- StmtElif.        Stmt ::= "elif" Exp Block ;
-- StmtElse.        Stmt ::= "else" Block ;
-- StmtWhile.       Stmt ::= "while" Exp  Block ;
-- StmtForIn.       Stmt ::= "for" Name "in" Exp Block ;
-- StmtForVarIn.    Stmt ::= "for" "->" Name "in" Exp Block ;
-- StmtCase.        Stmt ::= "case" [Exp] ":" "{" [CasePattern] "}";
-- internal StmtIfElse. Stmt ::= "if" Exp Block [ElifClause];

runPureExpr :: AST.Exp -> Env -> IO Value
runPureExpr (AST.ExpArrow _) _env = error "Pure expression expected, but -> found"
runPureExpr locExp@(AST.ExpVoT _) env = do
    return $ getLoc (expAsLoc locExp env) env
runPureExpr (AST.ExpInt i) _env = return $ ValInt $ fromInteger i
runPureExpr (AST.ExpAnd lexp rexp) env = runPureBinOp (wrapBoolOp (&&)) lexp rexp env
runPureExpr (AST.ExpOr lexp rexp) env = runPureBinOp (wrapBoolOp (||)) lexp rexp env
runPureExpr (AST.ExpCmp lexp cmpOp rexp) env = do
    runPureBinOp (cmpOpFun cmpOp) lexp rexp env
runPureExpr (AST.ExpAdd lexp rexp) env = runPureBinOp (wrapIntOp (+)) lexp rexp env
runPureExpr (AST.ExpSub lexp rexp) env = runPureBinOp (wrapIntOp (-)) lexp rexp env
runPureExpr (AST.ExpMul lexp rexp) env = runPureBinOp (wrapIntOp (*)) lexp rexp env
-- runPureExpr (AST.ExpDiv lexp rexp) env = runPureBinOp (/) lexp rexp env
runPureExpr (AST.ExpDivInt lexp rexp) env = runPureBinOp (wrapIntOp div) lexp rexp env
runPureExpr (AST.ExpMod lexp rexp) env = runPureBinOp (wrapIntOp mod) lexp rexp env
runPureExpr _expr@(AST.ExpCall fexp argExps) env = do
    function <- case fexp of
        AST.ExpVoT [AST.Name (_, fname)] -> return $ (functions env) M.! fname
        _ -> error $ "Not callable: " ++ (printTree fexp)
    argVals <- forM argExps $ \expr -> do
        runPureExpr expr env
    let argouts = map (const Nothing) argVals
    env' <- runFunction function env argVals argouts (Just $ varLoc "$")
    return $ getLoc (varLoc "$") env'
runPureExpr expr _env = error $ "runPureExpr not yet implemented for " ++ (printTree expr)

-- ExpAnd. Exp2 ::= Exp2 "and" Exp4 ;
--
-- ExpOr. Exp4 ::= Exp4 "or" Exp6 ;
--
-- ExpCmp. Exp6 ::= Exp6 CmpOp Exp8 ;
--
-- ExpAdd.   Exp8 ::= Exp8 "+" Exp42 ;
-- ExpSub.   Exp8 ::= Exp8 "-" Exp42 ;
--
-- ExpMul.    Exp42 ::= Exp42 "*" Exp50 ;
-- ExpDiv.    Exp42 ::= Exp42 "/" Exp50 ;
-- ExpDivInt. Exp42 ::= Exp42 "//" Exp50 ;
-- ExpMod.    Exp42 ::= Exp42 "%" Exp50 ;
--
-- ExpRange. Exp50 ::= Exp55 ".." Exp55 ;
--
-- ExpCall. Exp55 ::= Exp60 "(" [Exp] ")" ;
--
-- ExpInt.   Exp60 ::= Integer ;
-- ExpStr.   Exp60 ::= String ;
-- ExpArray. Exp60 ::= "[" [Exp] "]" ;
-- ExpTuple. Exp60 ::= "(" Exp "," [Exp] ")" ;
--
-- ExpNot.   Exp60 ::= "not" Exp60 ;
-- ExpNeg.   Exp60 ::= "-" Exp60 ;
--
-- ExpArrow.        Exp70 ::= "->" Exp71 ;
-- ExpVoT.       Exp71 ::= [Name] ; -- var or type
-- ExpSubscript. Exp71 ::= Exp71 "[" [Exp] "]" ;  -- index projection or generic instantiation

runNonPureExpr :: Maybe Loc -> AST.Exp -> Env -> IO Env
runNonPureExpr _ (AST.ExpArrow _) _env = error "Unexpected ->"
runNonPureExpr mdest expr@(AST.ExpCall fexp args) env = do
    function <- case fexp of
        AST.ExpVoT [AST.Name (_, fname)] -> return $ (functions env) M.! fname
        _ -> error $ "Not callable: " ++ (printTree fexp)
    (argExps, argouts) <- liftM unzip $ forM args (\arg -> do
            case arg of
                AST.ExpArrow arg' -> return (arg', Just $ expAsLoc arg' env)
                _ -> return (arg, Nothing)
        )
    when ((isNothing mdest) && (null $ catMaybes argouts)) $ error $ "Non-pure expression expected, but found pure expression " ++ (printTree expr)
    argVals <- forM argExps $ \e -> runPureExpr e env
    runFunction function env argVals argouts mdest
runNonPureExpr (Just dest) expr env = do
    val <- runPureExpr expr env
    return $ setLoc dest val env
runNonPureExpr Nothing expr@(AST.ExpAnd lexp rexp) env = runNonPureBinOp (wrapBoolOp (&&)) expr lexp rexp env
runNonPureExpr Nothing expr@(AST.ExpOr lexp rexp) env = runNonPureBinOp (wrapBoolOp (||)) expr lexp rexp env
runNonPureExpr Nothing expr@(AST.ExpCmp lexp cmpOp rexp) env = do
    runNonPureBinOp (cmpOpFun cmpOp) expr lexp rexp env
runNonPureExpr Nothing expr@(AST.ExpAdd lexp rexp) env = runNonPureBinOp (wrapIntOp (+)) expr lexp rexp env
runNonPureExpr Nothing expr@(AST.ExpSub lexp rexp) env = runNonPureBinOp (wrapIntOp (-)) expr lexp rexp env
runNonPureExpr Nothing expr@(AST.ExpMul lexp rexp) env = runNonPureBinOp (wrapIntOp (*)) expr lexp rexp env
-- runNonPureExpr Nothing expr@(AST.ExpDiv lexp rexp) env = runNonPureBinOp (/) expr lexp rexp env
runNonPureExpr Nothing expr@(AST.ExpDivInt lexp rexp) env = runNonPureBinOp (wrapIntOp div) expr lexp rexp env
runNonPureExpr Nothing expr@(AST.ExpMod lexp rexp) env = runNonPureBinOp (wrapIntOp mod) expr lexp rexp env
runNonPureExpr _ expr _env = error $ "Non-pure expression expected, but found pure expression " ++ (printTree expr)


runNonPureBinOp :: BinOpF -> AST.Exp -> AST.Exp -> AST.Exp -> Env -> IO Env
runNonPureBinOp op wholeExpr lexp rexp env = do
    (locExp, lexp', rexp') <- case (lexp, rexp) of
            (AST.ExpArrow _, AST.ExpArrow _) -> error $ "Multiple arrows in " ++ (printTree wholeExpr)
            (AST.ExpArrow lexp', _) -> return (lexp', lexp', rexp)
            (_, AST.ExpArrow rexp') -> return (rexp', lexp, rexp')
            _ -> error $ "runNonPureBinOp: non-pure expression expected, but no arrows found " ++ (printTree wholeExpr)
    let destLoc = expAsLoc locExp env
    val <- runPureBinOp op lexp' rexp' env
    return $ setLoc destLoc val env

runPureBinOp :: BinOpF -> AST.Exp -> AST.Exp -> Env -> IO Value
runPureBinOp op lexp rexp env = do
    lval <- runPureExpr lexp env
    rval <- runPureExpr rexp env
    return $ lval `op` rval

expAsLoc :: AST.Exp -> Env -> Loc
expAsLoc (AST.ExpVoT [AST.Name (_, varName)]) _env = Loc varName []  -- TODO: check existence
expAsLoc destExp _env = error $ "Not lvalue or not yet handled as lvalue: " ++ (printTree destExp)

wrapBoolOp :: (Bool -> Bool -> Bool) -> (Value -> Value -> Value)
wrapBoolOp op (ValBool b1) (ValBool b2) = ValBool $ b1 `op` b2
wrapBoolOp _ bad (ValBool _) = error $ "Not a bool: " ++ (show bad)
wrapBoolOp _ _ bad = error $ "Not a bool: " ++ (show bad)

wrapIntOp :: (Int -> Int -> Int) -> (Value -> Value -> Value)
wrapIntOp op (ValInt i1) (ValInt i2) = ValInt $ i1 `op` i2
wrapIntOp _ bad (ValInt _) = error $ "Not an integer: " ++ (show bad)
wrapIntOp _ _ bad = error $ "Not an integer: " ++ (show bad)

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
