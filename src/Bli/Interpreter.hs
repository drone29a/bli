module Bli.Interpreter where

import Bli.Ast (
  Environment (Environment, mapping),
  ErrorMsg,
  Stmt (..),
  eval,
  stringify, Var, Expr,
 )

import Prelude hiding (putStrLn)

import Control.Monad.State (MonadIO (..), StateT (runStateT), modify, gets)
import Data.Foldable (traverse_)
import Data.HashMap.Strict qualified as HMap
import Data.Text.IO (putStrLn)

data InterpreterState = InterpreterState
  { environment :: Environment
  , errors :: [ErrorMsg]
  }
  deriving (Eq, Show)

initialInterpreterState :: InterpreterState
initialInterpreterState =
  InterpreterState
    { environment = Environment HMap.empty
    , errors = []
    }

type Interpreter = StateT InterpreterState IO

runInterpreter :: StateT s m a -> s -> m (a, s)
runInterpreter = runStateT

interpret :: [Stmt] -> IO ()
interpret stmts = do
  (_, _finalState) <-
    runInterpreter
      (traverse_ execute stmts)
      initialInterpreterState
  return ()

-- interpret :: Expr -> IO ()
-- interpret expr = do
--   case eval expr of
--     Right result -> print $ stringify result
--     Left errorMsg -> print errorMsg

execute :: Stmt -> Interpreter ()
execute stmt = do
  env <- gets environment
  case stmt of
    StmtPrint expr ->
      case eval env expr of
        Right result -> liftIO $ putStrLn $ stringify result
        Left errorMsg -> addError errorMsg
    StmtDecl var expr -> defVar var expr
    StmtExpr expr ->
      case eval env expr of
        Right _result -> return ()
        Left errorMsg -> addError errorMsg

addError :: ErrorMsg -> Interpreter ()
addError errorMsg =
  modify (\s -> s{errors = errorMsg : errors s})

defVar :: Var -> Expr -> Interpreter ()
defVar var expr =
  modify
    ( \s ->
        s
          { environment =
              Environment
                ( HMap.insert
                    var
                    expr
                    (mapping . environment $ s)
                )
          }
    )

assignVar :: Var -> Expr -> Interpreter ()
assignVar var expr = undefined
-- This will be similar to defVar, but first needs to check if variable is defined?
-- Or does that matter, assignment becomes definition? Check Lox spec.