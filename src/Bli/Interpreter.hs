module Bli.Interpreter where

import Bli.Ast (
  Asgn (..),
  BinOp (..),
  Expr (..),
  Literal (..),
  Stmt (..),
  UnOp (..),
  Var (..),
  stringify, PossAsgn (PossAsgn), LVal (LValVar),
 )

import Prelude hiding (putStrLn)

import Control.Monad.Except (ExceptT, runExceptT, tryError, throwError)
import Control.Monad.State (MonadIO (..), StateT (runStateT), gets, modify)
import Data.Foldable (traverse_)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HMap
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO (putStrLn)
import Control.Monad (when)

newtype Environment = Environment
  { mapping :: HashMap Var Expr
  } deriving (Eq, Show)

lookupVar :: Environment -> Var -> Maybe Expr
lookupVar (Environment mapping) var =
  HMap.lookup var mapping

data InterpreterState = InterpreterState
  { environment :: Environment
  , errors :: [ErrorMsg]
  , debug :: Bool
  }
  deriving (Eq, Show)

initialInterpreterState :: InterpreterState
initialInterpreterState =
  InterpreterState
    { environment = Environment HMap.empty
    , errors = []
    , debug = True
    }

-- type Interpreter = StateT InterpreterState IO
type Interpreter = ExceptT ErrorMsg (StateT InterpreterState IO)

runInterpreter :: ExceptT e (StateT s IO) a -> s -> IO (Either e a, s)
runInterpreter = runStateT . runExceptT

interpret :: [Stmt] -> IO ()
interpret stmts = do
  (_, _finalState) <-
    runInterpreter
      (traverse_ execute stmts)
      initialInterpreterState
  return ()

interpretExpr :: Expr -> IO (Either ErrorMsg Expr)
interpretExpr expr = do
  (result, _finalState) <-
    runInterpreter (eval expr)
    initialInterpreterState
  return result

-- interpret :: Expr -> IO ()
-- interpret expr = do
--   case eval expr of
--     Right result -> print $ stringify result
--     Left errorMsg -> print errorMsg

execute :: Stmt -> Interpreter ()
execute stmt = do
  debugOn <- gets debug
  when debugOn (liftIO $ print stmt)
  case stmt of
    StmtPrint expr -> do
      result <- tryError $ eval expr
      case result of
        Right val -> liftIO $ putStrLn $ stringify val
        Left errorMsg -> addError errorMsg
    StmtDecl var expr -> defVar var expr
    StmtExpr expr -> do
      result <- tryError $ eval expr
      case result of
        Right _val -> return ()
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

newtype ErrorMsg = ErrorMsg Text
  deriving (Eq, Show)

mkErrorMsg :: Show a => a -> a -> ErrorMsg
mkErrorMsg found expected =
  ErrorMsg . T.pack $ "Found " <> show found <> ", but expected " <> show expected <> "."

-- | Recursively evaluate an expression.
eval :: Expr -> Interpreter Expr
eval expr = do
  case expr of
    ExprLit _lit -> return expr
    ExprUn op x -> evalUnary op x
    ExprBin op x y -> evalBinary op x y
    ExprGroup x -> eval x
    ExprVar var@(Var name) -> do
      env <- gets environment
      case lookupVar env var of
        Just val -> return val
        Nothing ->
          throwError
            ( ErrorMsg $
                "No variable named \'"
                  <> name
                  <> "\' found."
            )
    ExprAsgn (Asgn var@(LValVar (Var name)) expr) -> do
      val <- eval expr
      -- Perform assignment here
      return val
    ExprPossAsgn (PossAsgn _ _) ->
      throwError . ErrorMsg $ "Unexpected lingering assignment."

evalUnary :: UnOp -> Expr -> Interpreter Expr
evalUnary UnNeg x = do
  result <- eval x
  case result of
    ExprLit (LitNum val) -> return $ ExprLit (LitNum (-val))
    _ -> throwError $ mkErrorMsg (stringify result) "number"
evalUnary UnNot x = do
  result <- eval x
  return $ ExprLit (LitBool $ not . isTruthy $ result)

evalBinary :: BinOp -> Expr -> Expr -> Interpreter Expr
evalBinary op x y = do
  x' <- eval x
  y' <- eval y

  case op of
    BinEq -> do
      checkEq x' y'
      return $ ExprLit (LitBool $ x' == y')
    BinNeq -> do
      checkEq x' y'
      return $ ExprLit (LitBool $ x' /= y')
    BinLt -> do
      xNum <- getNum x'
      yNum <- getNum y'
      return $ ExprLit (LitBool $ xNum < yNum)
    BinLte -> do
      xNum <- getNum x'
      yNum <- getNum y'
      return $ ExprLit (LitBool $ xNum <= yNum)
    BinGt -> do
      xNum <- getNum x'
      yNum <- getNum y'
      return $ ExprLit (LitBool $ xNum > yNum)
    BinGte -> do
      xNum <- getNum x'
      yNum <- getNum y'
      return $ ExprLit (LitBool $ xNum >= yNum)
    BinAdd -> do
      xNum <- getNum x'
      yNum <- getNum y'
      return $ ExprLit (LitNum $ xNum + yNum)
    BinSub -> do
      xNum <- getNum x'
      yNum <- getNum y'
      return $ ExprLit (LitNum $ xNum - yNum)
    BinMul -> do
      xNum <- getNum x'
      yNum <- getNum y'
      return $ ExprLit (LitNum $ xNum * yNum)
    BinDiv -> do
      xNum <- getNum x'
      yNum <- getNum y'
      return $ ExprLit (LitNum $ xNum / yNum)
    BinLogAnd -> do
      xBool <- getBool x'
      yBool <- getBool y'
      return $ ExprLit (LitBool $ xBool && yBool)
    BinLogOr -> do
      xBool <- getBool x'
      yBool <- getBool y'
      return $ ExprLit (LitBool $ xBool || yBool)

checkEq :: Expr -> Expr -> Interpreter ()
checkEq x y =
  case (x, y) of
    (ExprLit (LitNum _), ExprLit (LitNum _)) -> return ()
    (ExprLit (LitStr _), ExprLit (LitStr _)) -> return ()
    (ExprLit (LitBool _), ExprLit (LitBool _)) -> return ()
    (ExprLit LitNil, _) -> return ()
    (_, ExprLit LitNil) -> return ()
    _ ->
      throwError . ErrorMsg $
        "Operands: " <> stringify x <> " and " <> stringify y <> "are not comparable."

getNum :: Expr -> Interpreter Float
getNum (ExprLit (LitNum x)) = return x
getNum expr =
  throwError . ErrorMsg $
    "Number expected but found: " <> stringify expr <> "."

getBool :: Expr -> Interpreter Bool
getBool (ExprLit (LitBool x)) = return x
getBool expr = return $ isTruthy expr

isTruthy :: Expr -> Bool
isTruthy (ExprLit LitNil) = False
isTruthy (ExprLit (LitBool x)) = x
isTruthy _ = True