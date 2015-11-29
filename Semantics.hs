module Semantics where

import Control.Monad
import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Maybe

-- import System.IO

import qualified Absfoo as AST
import Printfoo(printTree)

import Types


allocBlockName :: SM BlockName
allocBlockName = do
    n <- gets semNextFreeBlockName
    modify (\ss -> ss{semNextFreeBlockName=n+1})
    return n

inContext :: ContextLevel -> SM a -> SM a
inContext ctx tcm = do
    octx <- gets semContext
    modify (\ss -> ss{semContext=ctx:octx})
    result <- tcm
    modify (\ss -> ss{semContext=octx})
    return result

mkCtx :: (Int, Int) -> String -> String
mkCtx (line, col) s = (show line) ++ ":" ++ (show col) ++ ":" ++ s

reportError :: String -> SM ()
reportError errmsg = do
    st <- get
    let err = Err $ (show $ semContext st) ++ ": " ++ errmsg
    modify (\ss -> ss{semErrors=err:semErrors st})

modifyEnv :: (Env -> Env) -> SM ()
modifyEnv f = modify (\ss -> ss{semEnv=f $ semEnv ss})

modifyVars :: (Vars -> Vars) -> SM ()
modifyVars f = modifyEnv (\env -> env{vars=f $ vars env})

moduleSem :: AST.Module -> String -> Either [Err] Env
moduleSem (AST.Mod topDefs) _fileName = do
    ss <- return $ execState (do
                forM_ topDefs topDefSem
                mMain <- gets $ (M.lookup "main").functions.semEnv
                case mMain of
                    Just main -> do
                        when (argNames main /= []) $ do
                            reportError $ "invalid main() signature (main should not take any arguments)"
                        when (outArgs main /= []) $ do
                            reportError $ "invalid main() signature (main should not return anything)"
                    Nothing -> do
                        reportError "no main() procedure"
                fs <- gets $ functions . semEnv
                forM_ (M.elems fs) compile
            ) $ SemState [] [] initialEnv 1
    case semErrors ss of
        [] -> Right $ semEnv ss
        _ -> Left $ reverse $ semErrors ss
    where
        initialEnv = Env (M.fromList [("Int", Type)]) M.empty M.empty 0 M.empty

topDefSem :: AST.TopDef -> SM ()
topDefSem (AST.FunDef name genArgs argdefs mrettype (AST.Blk bodyStmts)) = do
    funDefSem False name genArgs argdefs mrettype bodyStmts
topDefSem (AST.ProcDef name genArgs argdefs mrettype (AST.Blk bodyStmts)) = do
    funDefSem True name genArgs argdefs mrettype bodyStmts
-- VariantTypeDef. TopDef ::= "type" Name GenArgsDef ":" "{" [VariantDef] "}" ;
-- SimpleTypeDef.  TopDef ::= "type" Name GenArgsDef ":" "{" [Field] "}" ;  -- just one variant
topDefSem topDef = reportError $ "topDefSem not yet implemented for " ++ (printTree topDef)

funDefSem :: Bool -> AST.Name -> AST.GenArgsDef -> [AST.ArgDef] -> AST.RetType -> [AST.Stmt] -> SM ()
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
            let fun = (Function {
                argNames=argnames,
                argLocs=map varLoc argnames,
                outArgs=outargs,
                outLocs=outlocs,
                body=undefined,
                isProc=proc,
                compile=inContext (mkCtx pos name) $ do
                    modifyVars $ const $ M.fromList $ map (\an -> (an, ())) argnames
                    bodyBlocks <- stmtsSem stmts 0 (BlockEnv Nothing Nothing Nothing)
                    updateFun fun{body=bodyBlocks}
--                     modifyVars $ const $ vars env
            })
            updateFun fun
        else do
            reportError $ "function redefined: " ++ name
        where
            updateFun fun = do
                modifyEnv (\env' -> env'{functions=M.insert name fun $ functions env'})

data BlockEnv = BlockEnv {
    blkNext :: Maybe BlockName,
    blkBreak :: Maybe BlockName,
    blkContinue :: Maybe BlockName
}

stmtsSem :: [AST.Stmt] -> BlockName -> BlockEnv -> SM QuadBlocks
stmtsSem stmts entryBlockName blkEnv = do
    stmts' <- fixIfs stmts
    stmtsSem' stmts' entryBlockName []
    where
        fixIfs :: [AST.Stmt] -> SM [AST.Stmt]
        fixIfs [] = return []
        fixIfs ((AST.StmtIf cond block):stmts') = do
            let (elifs, mElse, stmts'') = takeElifs stmts'
            stmts''' <- fixIfs stmts''
            elseBlk <- case mElse of
                    Just elseBlk -> return elseBlk
                    Nothing -> return $ AST.Blk []
            return $ (AST.StmtIfElse cond block elifs elseBlk):stmts'''
        fixIfs (AST.StmtElif{}:stmts'') = do
            reportError $ "Unexpected elif clause"
            fixIfs stmts''  -- possible more errors to report there :)
        fixIfs (AST.StmtElse{}:stmts'') = do
            reportError $ "Unexpected else clause"
            fixIfs stmts''  -- possible more errors to report there :)
        fixIfs (stmt:ss) = do
            rest <- fixIfs ss
            return $ stmt:rest
        takeElifs :: [AST.Stmt] -> ([AST.ElifClause], Maybe AST.Block, [AST.Stmt])
        takeElifs ((AST.StmtElif cond block):stmts') = do
            let (elifs, mElse, stmts'') = takeElifs stmts'
            ((AST.Elif cond block):elifs, mElse, stmts'')
        takeElifs ((AST.StmtElse block):stmts') = do
            ([], Just block, stmts')
        takeElifs stmts' = ([], Nothing, stmts')
        end :: BlockName -> QuadCode -> Jump -> SM QuadBlocks
        end curBlockName curBlockCode jump = do
            let currentBlock = (curBlockCode, jump)
            return $ M.fromList [(curBlockName, currentBlock)]
        stmtsSem' :: [AST.Stmt] -> BlockName -> QuadCode -> SM QuadBlocks
        stmtsSem' [] curBlockName curBlockCode = do
            end curBlockName curBlockCode $ case blkNext blkEnv of
                Just nextBlockName -> Jump nextBlockName
                Nothing -> Return
        stmtsSem' (AST.StmtPass:stmts') curBlockName curBlockCode = do
            stmtsSem' stmts' curBlockName curBlockCode
        stmtsSem' [AST.StmtReturn] curBlockName curBlockCode = do
            end curBlockName curBlockCode Return
        stmtsSem' (AST.StmtReturn:_:_) _ _ = do
            reportError "Dead code after return statement"
            return M.empty

        stmtsSem' [AST.StmtBreak] curBlockName curBlockCode = do
            case blkBreak blkEnv of
                 Just blockName -> end curBlockName curBlockCode (Jump blockName)
                 Nothing -> do
                    reportError $ "Illegal break statement"
                    return M.empty
        stmtsSem' (AST.StmtBreak:_:_) _ _ = do
            reportError "Dead code after break statement"
            return M.empty

        stmtsSem' [AST.StmtContinue] curBlockName curBlockCode = do
            case blkContinue blkEnv of
                 Just blockName -> end curBlockName curBlockCode (Jump blockName)
                 Nothing -> do
                    reportError $ "Illegal continue statement"
                    return M.empty
        stmtsSem' (AST.StmtContinue:_:_) _ _ = do
            reportError "Dead code after continue statement"
            return M.empty

--         stmtsSem' ((AST.StmtIfElse condExp (AST.Blk thenStmts) [] (AST.Blk [])):stmt') curBlockName curBlockCode = do
            -- TODO: optimize empty elifs and else block
        stmtsSem' ((AST.StmtIfElse condExp (AST.Blk thenStmts) elifs elseBlock@(AST.Blk elseStmts)):stmt') curBlockName curBlockCode = do
            mAfterBlockName <- do
                if not $ null stmt' then do
                    liftM Just $ allocBlockName
                else do
                    return $ blkNext blkEnv
            lBranch <- allocBlockName
            rBranch <- allocBlockName
            (condCode, condQVal) <- pureExprSem Nothing condExp
            previousBlocks <- end curBlockName (curBlockCode ++ condCode) (Branch condQVal lBranch rBranch)
            let innerBlkEnv = blkEnv{blkNext=mAfterBlockName}
            lBranchBlocks <- stmtsSem thenStmts lBranch innerBlkEnv
            rBranchBlocks <- case elifs of
                [] -> stmtsSem elseStmts rBranch innerBlkEnv
                (AST.Elif elifCond elifThen):elifs' -> do
                    stmtsSem [AST.StmtIfElse elifCond elifThen elifs' elseBlock] rBranch innerBlkEnv
            followingBlocks <- do
                if not $ null stmt' then do
                    stmtsSem' stmt' (fromJust mAfterBlockName) []
                else do
                    return M.empty
            return $ M.unions [previousBlocks, lBranchBlocks, rBranchBlocks, followingBlocks]
        stmtsSem' (stmt@(AST.StmtWhile {}):_) _ _ = do
            reportError $ "Statement not yet implemented: " ++ printTree stmt
            return M.empty
        stmtsSem' (stmt@(AST.StmtForIn {}):_) _ _ = do
            reportError $ "Statement not yet implemented: " ++ printTree stmt
            return M.empty
        stmtsSem' (stmt@(AST.StmtForVarIn {}):_) _ _ = do
            reportError $ "Statement not yet implemented: " ++ printTree stmt
            return M.empty
        stmtsSem' (stmt@(AST.StmtCase {}):_) _ _ = do
            reportError $ "Statement not yet implemented: " ++ printTree stmt
            return M.empty
        stmtsSem' [AST.StmtReturnValue valExp] curBlockName curBlockCode = do
            (exprQuads, _) <- pureExprSem (Just $ varLoc returnName) valExp
            end curBlockName (curBlockCode ++ exprQuads) Return
        stmtsSem' (AST.StmtReturnValue{}:_:_) _ _ = do
            reportError "Dead code after return statement"
            return M.empty
        stmtsSem' (noJumpStmt:stmts') curBlockName curBlockCode = do
            stmtCode <- inContext (stmtCtx noJumpStmt) $ stmtSem noJumpStmt
            stmtsSem' stmts' curBlockName (curBlockCode ++ stmtCode)

        stmtSem :: AST.Stmt -> SM QuadCode
        stmtSem (AST.StmtExp expr) = nonPureExprSem expr
        stmtSem (AST.StmtLAssign destExp call@(AST.ExpCall{})) = do
            mDestLoc <- case destExp of
                (AST.ExpVoT [AST.Name (_, "_")]) -> return Nothing
                _ -> liftM Just $ expAsLoc destExp
            callSem call (Just (mDestLoc, True))
        stmtSem (AST.StmtLAssign destExp valExp) = do
            destLoc <- expAsLoc destExp
            liftM fst $ pureExprSem (Just destLoc) valExp
        stmtSem (AST.StmtRAssign valExp destExp) = stmtSem (AST.StmtLAssign destExp valExp)
        stmtSem (AST.StmtPrint exprs) = do
            (exprsQuads, exprVals) <- liftM unzip $ mapM (pureExprSem Nothing) exprs
            return $ concat $ exprsQuads ++ [[QPrint exprVals]]
        stmtSem (AST.StmtAssert valExp) = do
            (exprQuads, exprVal) <- pureExprSem Nothing valExp
            return $ exprQuads ++ [QAssert exprVal (printTree valExp)]
        stmtSem stmt = do
            reportError $ "stmtSem: Statement not yet implemented: " ++ (printTree stmt)
            return []


stmtCtx :: AST.Stmt -> ContextLevel
stmtCtx = printTree

pureExprSem :: Maybe Loc -> AST.Exp -> SM (QuadCode, QVal)
pureExprSem mLoc e =
    inContext (exprCtx e) $ do
        pureExprSem' e
    where
        pureExprSem' (AST.ExpArrow _) = do
            reportError $ "pureExprSem: Pure expression expected, but arrow found"
            return ([], QValConst $ error $ "invalid")
        pureExprSem' (AST.ExpTrue) = literalSem mLoc $ ValBool True
        pureExprSem' (AST.ExpFalse) = literalSem mLoc $ ValBool False
        pureExprSem' (AST.ExpInt i) = literalSem mLoc $ ValInt $ fromInteger i
        pureExprSem' (AST.ExpStr s) = literalSem mLoc $ ValString s
        pureExprSem' locExp@(AST.ExpVoT _) = do
            loc <- expAsLoc locExp
            case mLoc of
                Just destLoc -> return ([Quad2 destLoc $ QValVar loc], QValVar destLoc)
                Nothing -> return ([], QValVar loc)
        pureExprSem' (AST.ExpAnd lExp rExp) = pureBinOpSem mLoc (wrapBoolOp (&&)) lExp rExp
        pureExprSem' (AST.ExpOr lExp rExp) = pureBinOpSem mLoc (wrapBoolOp (||)) lExp rExp
        pureExprSem' (AST.ExpCmp lExp cmpOp rExp) = pureBinOpSem mLoc (cmpOpFun cmpOp) lExp rExp
        pureExprSem' (AST.ExpAdd lExp rExp) = pureBinOpSem mLoc (wrapIntOp (+)) lExp rExp
        pureExprSem' (AST.ExpSub lExp rExp) = pureBinOpSem mLoc (wrapIntOp (-)) lExp rExp
        pureExprSem' (AST.ExpMul lExp rExp) = pureBinOpSem mLoc (wrapIntOp (*)) lExp rExp
--         pureExprSem' (AST.ExpDiv lExp rExp) = pureBinOpSem' True mLoc (/) lExp rExp
        pureExprSem' (AST.ExpDivInt lExp rExp) = pureBinOpSem' True mLoc (wrapIntOp div) lExp rExp
        pureExprSem' (AST.ExpMod lExp rExp) = pureBinOpSem mLoc (wrapIntOp mod) lExp rExp
        pureExprSem' call@(AST.ExpCall{}) = do
            destLoc <- case mLoc of
                Just loc -> return loc
                Nothing -> newTmpLoc
            quadCode <- callSem call (Just (Just destLoc, False))
            return (quadCode, QValVar destLoc)
        pureExprSem' expr = do
            reportError $ "pureExprSem: Expression not yet implemented: " ++ (printTree expr)
            return ([], QValConst $ error $ "invalid")

        literalSem Nothing val = return ([], QValConst val)
        literalSem (Just destLoc) val = return ([Quad2 destLoc $ QValConst val], QValVar destLoc)

pureBinOpSem :: Maybe Loc -> BinOpF -> AST.Exp -> AST.Exp -> SM (QuadCode, QVal)
pureBinOpSem = pureBinOpSem' False

pureBinOpSem' :: Bool -> Maybe Loc -> BinOpF -> AST.Exp -> AST.Exp -> SM (QuadCode, QVal)
pureBinOpSem' isDivision mLoc op lExp rExp = do
    (lCode, lQVal) <- pureExprSem Nothing lExp
    (rCode, rQVal) <- pureExprSem Nothing rExp
    case rQVal of
        QValConst rConst -> do
            when (isDivision && isZero rConst) $ do
                reportError $ "Division by 0"
        _ -> return ()
    case (lCode, lQVal, rCode, rQVal) of
        ([], QValConst lConst, [], QValConst rConst) -> do
            return ([], QValConst $ lConst `op` rConst)
        _ -> do
            destLoc <- case mLoc of
                Just loc -> return loc
                Nothing -> newTmpLoc
            return (lCode ++ rCode ++ [Quad4 destLoc lQVal op rQVal], QValVar destLoc)

nonPureExprSem :: AST.Exp -> SM QuadCode
nonPureExprSem e =
    inContext (exprCtx e) $ do
        nonPureExprSem' e
    where
        nonPureExprSem' (AST.ExpArrow _) = do
            reportError "Unexpected ->"
            return []
        nonPureExprSem' (AST.ExpAnd lExp rExp) = nonPureBinOpSem (wrapBoolOp (&&)) lExp rExp
        nonPureExprSem' (AST.ExpOr lExp rExp) = nonPureBinOpSem (wrapBoolOp (||)) lExp rExp
        nonPureExprSem' (AST.ExpCmp lExp cmpOp rExp) = nonPureBinOpSem (cmpOpFun cmpOp) lExp rExp
        nonPureExprSem' (AST.ExpAdd lExp rExp) = nonPureBinOpSem (wrapIntOp (+)) lExp rExp
        nonPureExprSem' (AST.ExpSub lExp rExp) = nonPureBinOpSem (wrapIntOp (-)) lExp rExp
        nonPureExprSem' (AST.ExpMul lExp rExp) = nonPureBinOpSem (wrapIntOp (*)) lExp rExp
--         nonPureExprSem' (AST.ExpDiv lExp rExp) = nonPureBinOpSem' True (/) lExp rExp
        nonPureExprSem' (AST.ExpDivInt lExp rExp) = nonPureBinOpSem' True (wrapIntOp div) lExp rExp
        nonPureExprSem' (AST.ExpMod lExp rExp) = nonPureBinOpSem (wrapIntOp mod) lExp rExp
        nonPureExprSem' call@(AST.ExpCall{}) = do
            callSem call Nothing
        nonPureExprSem' expr = do
            reportError $ "nonPureExprSem: Expression not yet implemented: " ++ (printTree expr)
            return []

nonPureBinOpSem :: BinOpF -> AST.Exp -> AST.Exp -> SM QuadCode
nonPureBinOpSem = nonPureBinOpSem' False

nonPureBinOpSem' :: Bool -> BinOpF -> AST.Exp -> AST.Exp -> SM QuadCode
nonPureBinOpSem' isDivision op lExp rExp = do
    mTriple <- case (lExp, rExp) of
            (AST.ExpArrow _, AST.ExpArrow _) -> do
                reportError $ "Multiple arrows"
                return Nothing
            (AST.ExpArrow lExp', _) -> return $ Just (lExp', lExp', rExp)
            (_, AST.ExpArrow rExp') -> return $ Just (rExp', lExp, rExp')
            _ -> do
                reportError $ "nonPureBinOpSem: non-pure expression expected, but no top-level arrows found"
                return Nothing
    case mTriple of
        Just (locExp, lExp', rExp') -> do
            destLoc <- expAsLoc locExp
            liftM fst $ pureBinOpSem' isDivision (Just destLoc) op lExp' rExp'
        Nothing -> return []  -- error already reported above

callSem :: AST.Exp -> Maybe (Maybe Loc, Bool) -> SM QuadCode
callSem (AST.ExpCall (AST.ExpVoT [AST.Name (_pos, fName)]) argExps) outsInfo = do
    mf <- liftM (M.lookup fName) $ gets $ functions . semEnv
    case mf of
        Just f -> do
            (argPairs, mDestMLocs) <- liftM unzip $ mapM extractArg $ argExps `zip` argNames f
            let (argCodes, argQVals) = unzip argPairs
                argsCode = concat argCodes
                (destArgs, destMLocs) = unzip $ catMaybes mDestMLocs
                destArgs' = (maybeToList (outsInfo >> return returnName)) ++ destArgs
            -- reports errors, if any; reporting does not affect returned code
            let retOuts = map fst $ maybeToList outsInfo
                destMLocs' = retOuts ++ destMLocs
            case (outsInfo, destArgs, outArgs f) of
                (Just (_, False), _:_, _) -> do
                    reportError $ "callSem: Pure expression expected, but arrow found"
                (Nothing, [], []) -> when (not $ isProc f) $ do
                    reportError $ "callSem: Function result ignored"
                _ -> do
                    when (length destArgs' /= 1 && (length $ outArgs f) /= 1 &&
                            destArgs' /= outArgs f) $ do
                        reportError $ "callSem: Signature mismatch: bad arrows"
                    when ((all isNothing destMLocs') && (not $ isProc f)) $ do
                        reportError $ "callSem: Function result ignored"
            let argsProvided = length argQVals
                argsExpected = length (argNames f)
            when (argsProvided /= argsExpected) $ do
                reportError $ "Bad number of arguments for function `" ++ fName ++
                    "' (" ++ show argsExpected ++ " expected, " ++
                    show argsProvided ++ " found)"
            return $ argsCode ++ [QCall fName argQVals destMLocs']
        Nothing -> do
            reportError $ "Function undefined: " ++ fName
            return []
    where
        extractArg (AST.ExpArrow expr, argName) = do
            argPair <- pureExprSem Nothing expr
            destLoc <- expAsLoc expr
            return (argPair, Just (argName, Just destLoc))
        extractArg (expr, _argName) = do
            argPair <- pureExprSem Nothing expr
            return (argPair, Nothing)
callSem fExp@(AST.ExpCall{}) _ = do
    reportError $ "Calling this is illegal or not yet supported: " ++ (printTree fExp)
    return []
callSem notACall _ = do
    error $ "callSem called with some crap instead of AST.ExpCall: " ++ (printTree notACall)

exprCtx :: AST.Exp -> ContextLevel
exprCtx = printTree

expAsLoc :: AST.Exp -> SM Loc
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
