module Bli.Interpreter where

import Bli.Ast (
  Asgn (..),
  BinOp (..),
  Expr (..),
  LVal (LValVar),
  Literal (..),
  Stmt (..),
  UnOp (..),
  Var (..),
  stringify, PExpr, UnExpr (UnExpr), BinExpr (BinExpr),
 )

import Prelude hiding (putStrLn)

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (MonadIO (..), StateT (runStateT), gets, modify)
import Data.Foldable (traverse_)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HMap
import Data.Text.IO (putStrLn)
import Control.Monad (when)
import Bli.Analysis (parseExpr)
import Bli.Error (ErrorMsg (ErrorMsg), mkErrorMsg)
import Data.Maybe (mapMaybe)
import Control.Applicative ((<|>))


type Mapping = HashMap Var Expr

newtype GlobalEnv = GlobalEnv {gMapping :: Mapping}
  deriving (Eq, Show)

newtype LocalEnv = LocalEnv {lMapping :: Mapping}
  deriving (Eq, Show)

-- | Recursively check for the `Var` value in the local
-- environments. Once the nested local environments are exhausted,
-- check the global environment.
lookupVar :: GlobalEnv -> [LocalEnv] -> Var -> Maybe Expr
lookupVar gEnv lEnvs var =
  lookupLocalVar lEnvs var <|> HMap.lookup var (gMapping gEnv)

lookupLocalVar :: [LocalEnv] -> Var -> Maybe Expr
lookupLocalVar envs var =
  case mapMaybe (HMap.lookup var . lMapping) envs of
    [] -> Nothing
    x : _ -> Just x

-- | There is aloways a base global environment
-- and there is always an active environment used for
-- assignments.
--
-- Any time a new scope is entered a new local
-- environment is created and will reference the previously
-- active environment as its parent environment.
-- The `environment` field should always reference the most-nested
-- environment. Whenever entering or exiting a scoped block, the
-- `environment` will be modified.
-- 
-- We could use a single `Environment` type to capture both 
-- the global and local environments, but then we could
-- potentially represent unintended nestings of environments.
data InterpreterState = InterpreterState
  { globalEnv :: GlobalEnv
  , localEnvs :: [LocalEnv]
  , errors :: [ErrorMsg]
  , debug :: Bool
  }
  deriving (Eq, Show)

initialInterpreterState :: InterpreterState
initialInterpreterState =
  InterpreterState
    { globalEnv = GlobalEnv HMap.empty
    , localEnvs = []
    , errors = []
    , debug = True
    }

-- type Interpreter = StateT InterpreterState IO
type Interpreter = ExceptT ErrorMsg (StateT InterpreterState IO)

runInterpreter :: ExceptT e (StateT s IO) a -> s -> IO (Either e a, s)
runInterpreter = runStateT . runExceptT

interpret :: [Stmt Expr] -> IO ()
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

execute :: Stmt Expr -> Interpreter ()
execute stmt = do
  debugOn <- gets debug
  when debugOn (liftIO $ print stmt)
  case stmt of
    StmtPrint expr -> do
      -- MBR: Do we want to propagate error up or 
      --      handle it by logging?
      -- result <- tryError $ eval expr
      -- case result of
      --   Right val -> liftIO $ putStrLn $ stringify val
      --   Left errorMsg -> addError errorMsg
      result <- eval expr
      liftIO . putStrLn $ stringify result
    StmtDecl var expr -> do
      result <- eval expr
      defVar var result
    StmtExpr expr -> do
      _ <- eval expr
      return ()
    StmtBlock stmts -> do
      modify (\s -> s{localEnvs = LocalEnv HMap.empty : localEnvs s})
      traverse_ execute stmts
      _headEnv : restEnvs <- gets localEnvs
      modify (\s -> s{localEnvs = restEnvs})
    StmtIf cond tBody fBodyOpt -> do
      result <- eval cond
      if isTruthy result then
        execute tBody
      else
        traverse_ execute fBodyOpt
      return ()

process :: [Stmt PExpr] -> [Stmt Expr]
process stmts =
  let result = traverse (traverse parseExpr) stmts in
    case result of
      Left _ -> []
      Right stmts' -> stmts'

addError :: ErrorMsg -> Interpreter ()
addError errorMsg =
  modify (\s -> s{errors = errorMsg : errors s})

defVar :: Var -> Expr -> Interpreter ()
defVar var expr =
  modify
    ( \s ->
        case s of
          -- Handle the case where there is an active
          -- local environment.
          InterpreterState{localEnvs = (LocalEnv m) : lEnvs} ->
            s
              { localEnvs = LocalEnv (HMap.insert var expr m) : lEnvs
              }
          -- Handle the case where there is no active
          -- local environment.
          InterpreterState
            { globalEnv = GlobalEnv m
            , localEnvs = []
            } ->
              s
                { globalEnv = GlobalEnv (HMap.insert var expr m)
                }
    )

-- | Assign a new value to an existing variable. If the variable does not exist,
-- then the assignment shall _not_ define a new variable with the name.
assignVar :: Var -> Expr -> Interpreter ()
assignVar var@(Var vName) val = do
  gEnv <- gets globalEnv
  lEnvs <- gets localEnvs
  case assignLocalVar lEnvs var val of
    Just lEnvs' -> do
      modify ( \s -> s{localEnvs = lEnvs'})
    Nothing -> case assignGlobalVar gEnv var val of
      Just gEnv' -> do
        modify ( \s -> s{globalEnv = gEnv'})
      Nothing -> 
        throwError . ErrorMsg $ 
          "No variable (" <> vName <> ") found for assignment."

assignLocalVar :: [LocalEnv] -> Var -> Expr -> Maybe [LocalEnv]
assignLocalVar envs var val =
  case remEnvs of
    x : xs -> Just $ preEnvs ++ [x{lMapping = HMap.insert var val (lMapping x)}] ++ xs
    _ -> Nothing
    where
      (preEnvs, remEnvs) = break (HMap.member var . lMapping) envs

assignGlobalVar :: GlobalEnv -> Var -> Expr -> Maybe GlobalEnv
assignGlobalVar env var val =
  if HMap.member var (gMapping env) then
    Just $ env{gMapping = HMap.insert var val (gMapping env)}
  else
    Nothing



-- | Recursively evaluate an expression.
eval :: Expr -> Interpreter Expr
eval expr = do
  case expr of
    ExprLit _lit -> return expr
    ExprUn (UnExpr op x) -> evalUnary op x
    ExprBin (BinExpr op x y) -> evalBinary op x y
    ExprGroup x -> eval x
    ExprVar var@(Var name) -> do
      gEnv <- gets globalEnv
      lEnvs <- gets localEnvs
      case lookupVar gEnv lEnvs var of
        Just val -> return val
        Nothing ->
          throwError
            ( ErrorMsg $
                "No variable named \'"
                  <> name
                  <> "\' found."
            )
    ExprAsgn (Asgn (LValVar var) right) -> do
      val <- eval right
      assignVar var val
      return val

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