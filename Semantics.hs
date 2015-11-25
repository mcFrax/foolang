module Semantics where

-- import Control.DeepSeq
import Control.Monad
-- import Control.Monad.Reader.Class
-- import Control.Monad.State.Class
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
showSemError (Err s) = "DUPA: " ++ s

data GlobEnv = GlobEnv {
    functions :: M.Map String Function,
    types :: M.Map String Type,
    variants :: M.Map String String  -- variant -> type
}

type LocEnv = M.Map String Value

data Function = Function {
    argNames :: [String],
    returns :: Bool,
    outArgs :: [String],
    body :: [AST.Stmt]
}

data Type = Type

type Value = Int

moduleSem :: AST.Module -> String -> Either Err (IO ())
moduleSem (AST.Mod topDefs) _fileName = do
    let env = foldl topDefSem (GlobEnv M.empty (M.fromList [("Int", Type)]) M.empty) topDefs
    return $ do
        _ <- runFunction (functions env M.! "main") (env, M.empty) [] [] Nothing
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
topDefSem _ _ = error "topDefSem: dupa"

runFunction :: Function -> (GlobEnv, LocEnv) -> [Value] -> [Maybe String] -> Maybe String -> IO LocEnv
runFunction f (genv, olenv) args argouts mout = do
    let ilenv = M.fromList $ argNames f `zip` args
    (ilenv', mret) <- runStmts (body f) genv ilenv
    let updates = case (outArgs f, returns f) of
            ([], False) -> []  -- no value returned
            ([], True) -> do  -- value returned, but no out args
                case (mret, mout, catMaybes argouts) of
                    (Nothing, Nothing, []) -> []
                    (Just _, Nothing, []) -> [] -- ignoring result - may be an error
                    (Just result, Just vname, []) -> [(vname, result)]
                    (Just result, Nothing, [vname]) -> [(vname, result)]
                    (Just _, _, _) -> error "Many places for one result? Fuck you!"
                    (Nothing, _, _) -> error "No value returned althought expected"
            ([oan], False) -> do  -- exactly one out arg in this function
                let retval = ilenv' M.! oan
                case (mret, mout, catMaybes argouts) of
                    (Nothing, Nothing, []) -> [] -- ignoring result - may be an error
                    (Nothing, Nothing, [vname]) -> [(vname, retval)]
                    (Nothing, Just vname, []) -> [(vname, retval)]
                    (Nothing, _, _) -> error "Many places for one result? Fuck you!"
                    (Just _, _, _) -> error "Unexpected return"
            (_oans, _) -> do
                let retupdate = case (returns f, mret, mout) of
                        (True, Just result, Just vname) -> [(vname, result)]
                        (False, Nothing, Nothing) -> []
                        (_, _, _) -> error "Screw you! That doesn't make sense!"
                    unpack (Just oenvName, value) = Just (oenvName, value)
                    unpack (Nothing, _) = Nothing
                retupdate ++ mapMaybe unpack (argouts `zip` (map (ilenv' M.!) $ argNames f))
    return $ M.fromList updates `M.union` olenv

runStmts :: [AST.Stmt] -> GlobEnv -> LocEnv -> IO (LocEnv, Maybe Value)
runStmts stmts _genv lenv = do
    putStrLn "LOL"
    forM_ stmts $ putStrLn . printTree
    return (lenv, Nothing)
