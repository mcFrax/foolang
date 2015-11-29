module QuadExecution where

import Control.Monad
import Control.Monad.State
import qualified Data.Map.Strict as M

import Types

runModule :: Env -> IO ()
runModule env = do
    _ <- execStateT (execQuad $ QCall "main" [] []) $ RunEnv env M.empty
    return ()

logQuad :: String -> Run ()
logQuad = const $ return ()
-- logQuad = liftIO . putStrLn

execQuadBlock :: QuadBlocks -> BlockName -> Run ()
execQuadBlock quadBlocks entryBlockName = do
    let (code, jump) = quadBlocks M.! entryBlockName
    forM_ code $ \quad -> do
        logQuad $ show quad
        execQuad quad
    case jump of
        Return -> do
            logQuad $ "Return"
            return ()
        Jump blockName -> do
            logQuad $ "Jump: " ++ show blockName
            execQuadBlock quadBlocks blockName
        Branch qVal lBranchName rBranchName -> do
            ValBool cond <- execQVal qVal
            let blockName = if cond then lBranchName
                                    else rBranchName
            logQuad $ "Branch: " ++ show blockName
            execQuadBlock quadBlocks blockName

execQuad :: Quad -> Run ()
execQuad (QPrint qvals) = do
    forM_ qvals $ \qval -> do
        s <- liftM ((++ " ") . showVal) $ execQVal qval
        liftIO $ putStr s
    liftIO $ putStrLn ""
execQuad (QAssert qval message) = do
    val <- execQVal qval
    case val of
            ValBool True -> return ()
            ValBool False -> error $ "Assertion failed: " ++ message
            _ -> error $ "Assertion value is not a bool: " ++ show val
execQuad (QCall fname args outs) = do
    re <- get
    let f = (functions $ staticEnv re) M.! fname
    argVals <- mapM execQVal args
    let argVars = M.fromList $ argNames f `zip` argVals
    put re{varVals=argVars}
    execQuadBlock (body f) 0
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

execQVal :: QVal -> Run Value
execQVal (QValConst val) = return val
execQVal (QValVar loc) = do
    liftM (getLoc loc) $ get
