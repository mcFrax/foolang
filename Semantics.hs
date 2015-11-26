module Semantics where

-- import Control.DeepSeq
import Control.Monad
-- import Control.Monad.Reader.Class
-- import Control.Monad.State.Class
-- import qualified Data.List as L
import Data.Bits
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
showSemError (Err s) = "DUPA: " ++ s

data GlobEnv = GlobEnv {
    functions :: M.Map String Function,
    types :: M.Map String Type,
    variants :: M.Map String String  -- variant -> type
}

type LocEnv = M.Map Loc Value

newtype Loc = Loc String deriving (Eq, Ord, Show)

data Function = Function {
    argNames :: [String],
    returns :: Bool,
    outArgs :: [String],
    body :: [AST.Stmt]
}

data Type = Type

type Value = Int

type BinOpF = (Value -> Value -> Value)

moduleSem :: AST.Module -> String -> Either Err (IO ())
moduleSem (AST.Mod topDefs) _fileName = do
    let env = foldl topDefSem (GlobEnv M.empty (M.fromList [("Int", Type)]) M.empty) topDefs
    return $ do
        _ <- runFunction (functions env M.! "main") env M.empty [] [] Nothing
        return ()

topDefSem :: GlobEnv -> AST.TopDef -> GlobEnv
topDefSem genv (AST.FunDef (AST.Name (_, name)) AST.NoGenArgs argdefs mrettype (AST.Blk fbody)) = do
    let (argnames, outargMaybes) = unzip $ map (\ad -> case ad of
                AST.ValArgDef _type (AST.Name (_, argName)) -> (argName, Nothing)
                AST.VarArgDef _type (AST.Name (_, argName)) -> (argName, Just argName)
            ) argdefs
        freturns = case mrettype of
            AST.JustType _type -> True
            AST.NoType -> False
    genv{functions=M.insert name (Function {
            argNames=argnames,
            returns=freturns,
            outArgs=catMaybes outargMaybes,
            body=fbody
        }) (functions genv)}
-- VariantTypeDef. TopDef ::= "type" Name GenArgsDef ":" "{" [VariantDef] "}" ;
-- SimpleTypeDef.  TopDef ::= "type" Name GenArgsDef ":" "{" [Field] "}" ;  -- just one variant
topDefSem _ topDef = error $ "topDefSem not yet implemented for " ++ (printTree topDef)

runFunction :: Function -> GlobEnv -> LocEnv -> [Value] -> [Maybe Loc] -> Maybe Loc -> IO LocEnv
runFunction f genv olenv args argouts mout = do
    let ilenv = M.fromList $ (map Loc $ argNames f) `zip` args
    (ilenv', mret) <- runStmts (body f) genv ilenv
    let updates = case ((map Loc $ outArgs f), returns f) of
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
                let retval = ilenv' M.! oan
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
                retupdate ++ mapMaybe unpack (argouts `zip` (map (ilenv' M.!) $ (map Loc $ argNames f)))
    return $ M.fromList updates `M.union` olenv

runStmts :: [AST.Stmt] -> GlobEnv -> LocEnv -> IO (LocEnv, Maybe Value)
runStmts stmts genv lenv = do
    (lenv', mmret) <- foldM runStmtOrReturn (lenv, Nothing) stmts
    let mret = (mmret >>= id) -- Nothing->Nothing|Just x->x
    return (lenv', mret)
    where
        runStmtOrReturn (lenv'', Just retval) _ = return (lenv'', Just retval)
        runStmtOrReturn (lenv'', Nothing) stmt = runStmt stmt genv lenv''

runStmt :: AST.Stmt -> GlobEnv -> LocEnv -> IO (LocEnv, Maybe (Maybe Value))
runStmt (AST.StmtPass) _genv lenv = return (lenv, Nothing)
runStmt (AST.StmtReturn) _genv lenv = return (lenv, Just Nothing)
runStmt (AST.StmtReturnValue expr) genv lenv = do
    val <- runPureExpr expr genv lenv
    return (lenv, Just $ Just val)
runStmt (AST.StmtPrint exprs) genv lenv = do
    forM_ exprs $ \expr -> do
        val <- runPureExpr expr genv lenv
        putStr $ show val ++ " "
    putStrLn ""
    return (lenv, Nothing)
runStmt (AST.StmtAssert expr) genv lenv = do
    val <- runPureExpr expr genv lenv
    when (val == 0) $ error $ "Assertion failed: " ++ (printTree expr)
    return (lenv, Nothing)
-- runStmt (AST.StmtLet locExp expr) genv lenv = do
--     let destLoc = expAsLoc locExp genv lenv
--     when (destLoc `M.member` lenv) $ error $ "Variable redefined: " ++ (show destLoc)
--     val <- runPureExpr expr genv lenv
--     return (M.insert destLoc val lenv, Nothing)
runStmt (AST.StmtLAssign locExp expr) genv lenv = do
    let destLoc = expAsLoc locExp genv lenv
--     when (destLoc `M.notMember` lenv) $ error $ "Variable undefined: " ++ (show destLoc)
    lenv' <- runNonPureExpr (Just destLoc) expr genv lenv
    return (lenv', Nothing)
runStmt (AST.StmtRAssign expr locExp) genv lenv = do
    runStmt (AST.StmtLAssign locExp expr) genv lenv
runStmt (AST.StmtExp expr) genv lenv = do
    lenv' <- runNonPureExpr Nothing expr genv lenv
    return (lenv', Nothing)
runStmt stmt _genv _lenv = error $ "runStmt not yet implemented for " ++ (printTree stmt)
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

runPureExpr :: AST.Exp -> GlobEnv -> LocEnv -> IO Value
runPureExpr (AST.ExpArrow _) _genv _lenv = error "Pure expression expected, but -> found"
runPureExpr locExp@(AST.ExpVoT _) genv lenv = do
    let loc = expAsLoc locExp genv lenv
    case M.lookup loc lenv of
         Just val -> return val
         Nothing -> error $ "No such variable: " ++ (show loc)
runPureExpr (AST.ExpInt i) _genv _lenv = return $ fromInteger i
runPureExpr (AST.ExpAnd lexp rexp) genv lenv = runPureBinOp (.&.) lexp rexp genv lenv
runPureExpr (AST.ExpOr lexp rexp) genv lenv = runPureBinOp (.|.) lexp rexp genv lenv
runPureExpr (AST.ExpCmp lexp cmpOp rexp) genv lenv = do
    runPureBinOp op lexp rexp genv lenv
    where
        op x y = if cmpOpFun cmpOp x y then 1 else 0
runPureExpr (AST.ExpAdd lexp rexp) genv lenv = runPureBinOp (+) lexp rexp genv lenv
runPureExpr (AST.ExpSub lexp rexp) genv lenv = runPureBinOp (-) lexp rexp genv lenv
runPureExpr (AST.ExpMul lexp rexp) genv lenv = runPureBinOp (*) lexp rexp genv lenv
-- runPureExpr (AST.ExpDiv lexp rexp) genv lenv = runPureBinOp (/) lexp rexp genv lenv
runPureExpr (AST.ExpDivInt lexp rexp) genv lenv = runPureBinOp (div) lexp rexp genv lenv
runPureExpr (AST.ExpMod lexp rexp) genv lenv = runPureBinOp (mod) lexp rexp genv lenv
runPureExpr _expr@(AST.ExpCall fexp argExps) genv lenv = do
    function <- case fexp of
        AST.ExpVoT [AST.Name (_, fname)] -> return $ (functions genv) M.! fname
        _ -> error $ "Not callable: " ++ (printTree fexp)
    argVals <- forM argExps $ \expr -> do
        runPureExpr expr genv lenv
    let argouts = map (const Nothing) argVals
    lenv' <- runFunction function genv lenv argVals argouts (Just resloc)
    return $ lenv' M.! resloc
    where
        resloc = Loc "$"
runPureExpr expr _genv _lenv = error $ "runPureExpr not yet implemented for " ++ (printTree expr)

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

runNonPureExpr :: Maybe Loc -> AST.Exp -> GlobEnv -> LocEnv -> IO LocEnv
runNonPureExpr _ (AST.ExpArrow _) _genv _lenv = error "Unexpected ->"
runNonPureExpr mdest expr@(AST.ExpCall fexp args) genv lenv = do
    function <- case fexp of
        AST.ExpVoT [AST.Name (_, fname)] -> return $ (functions genv) M.! fname
        _ -> error $ "Not callable: " ++ (printTree fexp)
    (argExps, argouts) <- liftM unzip $ forM args (\arg -> do
            case arg of
                AST.ExpArrow arg' -> return (arg', Just $ expAsLoc arg' genv lenv)
                _ -> return (arg, Nothing)
        )
    when ((isNothing mdest) && (null $ catMaybes argouts)) $ error $ "Non-pure expression expected, but found pure expression " ++ (printTree expr)
    argVals <- forM argExps $ \e -> runPureExpr e genv lenv
    runFunction function genv lenv argVals argouts mdest
runNonPureExpr (Just dest) expr genv lenv = do
    val <- runPureExpr expr genv lenv
    return $ M.insert dest val lenv
runNonPureExpr Nothing expr@(AST.ExpAnd lexp rexp) genv lenv = runNonPureBinOp (.&.) expr lexp rexp genv lenv
runNonPureExpr Nothing expr@(AST.ExpOr lexp rexp) genv lenv = runNonPureBinOp (.|.) expr lexp rexp genv lenv
runNonPureExpr Nothing expr@(AST.ExpCmp lexp cmpOp rexp) genv lenv = do
    runNonPureBinOp op expr lexp rexp genv lenv
    where
        op x y = if cmpOpFun cmpOp x y then 1 else 0
runNonPureExpr Nothing expr@(AST.ExpAdd lexp rexp) genv lenv = runNonPureBinOp (+) expr lexp rexp genv lenv
runNonPureExpr Nothing expr@(AST.ExpSub lexp rexp) genv lenv = runNonPureBinOp (-) expr lexp rexp genv lenv
runNonPureExpr Nothing expr@(AST.ExpMul lexp rexp) genv lenv = runNonPureBinOp (*) expr lexp rexp genv lenv
-- runNonPureExpr Nothing expr@(AST.ExpDiv lexp rexp) genv lenv = runNonPureBinOp (/) expr lexp rexp genv lenv
runNonPureExpr Nothing expr@(AST.ExpDivInt lexp rexp) genv lenv = runNonPureBinOp (div) expr lexp rexp genv lenv
runNonPureExpr Nothing expr@(AST.ExpMod lexp rexp) genv lenv = runNonPureBinOp (mod) expr lexp rexp genv lenv
runNonPureExpr _ expr _genv _lenv = error $ "Non-pure expression expected, but found pure expression " ++ (printTree expr)


runNonPureBinOp :: BinOpF -> AST.Exp -> AST.Exp -> AST.Exp -> GlobEnv -> LocEnv -> IO LocEnv
runNonPureBinOp op wholeExpr lexp rexp genv lenv = do
    (locExp, lexp', rexp') <- case (lexp, rexp) of
            (AST.ExpArrow _, AST.ExpArrow _) -> error $ "Multiple arrows in " ++ (printTree wholeExpr)
            (AST.ExpArrow lexp', _) -> return (lexp', lexp', rexp)
            (_, AST.ExpArrow rexp') -> return (rexp', lexp, rexp')
            _ -> error $ "runNonPureBinOp: non-pure expression expected, but no arrows found " ++ (printTree wholeExpr)
    let destLoc = expAsLoc locExp genv lenv
    val <- runPureBinOp op lexp' rexp' genv lenv
    return $ M.insert destLoc val lenv

runPureBinOp :: BinOpF -> AST.Exp -> AST.Exp -> GlobEnv -> LocEnv -> IO Value
runPureBinOp op lexp rexp genv lenv = do
    lval <- runPureExpr lexp genv lenv
    rval <- runPureExpr rexp genv lenv
    return $ lval `op` rval

expAsLoc :: AST.Exp -> GlobEnv -> LocEnv -> Loc
expAsLoc (AST.ExpVoT [AST.Name (_, varName)]) _genv _lenv = Loc varName  -- TODO: check existence
expAsLoc destExp _genv _lenv = error $ "Not lvalue or not yet handled as lvalue: " ++ (printTree destExp)

cmpOpFun :: AST.CmpOp -> (Value -> Value -> Bool)
cmpOpFun AST.EqOp = (==)
cmpOpFun AST.NeOp = (/=)
cmpOpFun AST.LtOp = (<)
cmpOpFun AST.GtOp = (>)
cmpOpFun AST.LeOp = (<=)
cmpOpFun AST.GeOp = (>=)
