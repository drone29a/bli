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
  stringify, PExpr, UnExpr (UnExpr), BinExpr (BinExpr), GlobalEnv (..), LocalEnv (..), Func (..),
 )

import Prelude hiding (putStr, putStrLn)

import Control.Monad.Except (ExceptT, runExceptT, throwError, MonadError, catchError)
import Control.Monad.State (MonadIO (..), StateT (runStateT), gets, modify, MonadState)
import Data.Foldable (traverse_)
import Data.HashMap.Strict qualified as HMap
import Data.Text.IO (putStr)
import Control.Monad (when, void, unless)
import Bli.Analysis (parseExpr)
import Bli.Error (BliException (ErrorMsg, Goto), mkErrorMsg)
import Data.Maybe (mapMaybe)
import Control.Applicative ((<|>))
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (stdout, hFlush)
import Control.Monad.Loops (whileM_)
import Data.IORef (readIORef, newIORef, writeIORef)

-- | Recursively check for the `Var` value in the local
-- environments. Once the nested local environments are exhausted,
-- check the global environment.
lookupVar :: GlobalEnv -> [LocalEnv] -> Var -> Interpreter (Maybe Expr)
lookupVar gEnv lEnvs var = do
  lVal <- lookupLocalVar lEnvs var
  gVal <- traverse (liftIO . readIORef) (HMap.lookup var (gMapping gEnv))
  return $ lVal <|> gVal

lookupLocalVar :: [LocalEnv] -> Var -> Interpreter (Maybe Expr)
lookupLocalVar envs var =
  case mapMaybe (HMap.lookup var . lMapping) envs of
    [] -> return Nothing
    x : _ -> do
      x' <- liftIO $ readIORef x
      return $ Just x'

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
  , errors :: [BliException]
  , debug :: Bool
  , collectOutput :: Bool
  , output :: [Text]
  , returnVal :: Expr -- MBR: This could be a Maybe Expr?
  }
  deriving (Eq, Show)

initialInterpreterState :: InterpreterState
initialInterpreterState =
  InterpreterState
    { globalEnv = GlobalEnv HMap.empty
    , localEnvs = []
    , errors = []
    , debug = True
    , collectOutput = True
    , output = []
    , returnVal = ExprLit LitNil
    }

newtype Interpreter a = Interpreter
  { unInterpreter :: ExceptT BliException (StateT InterpreterState IO) a
  }
  deriving (Functor)
  deriving newtype
    ( Applicative
    , Monad
    , MonadFail
    , MonadError BliException
    , MonadIO
    , MonadState InterpreterState
    )

runInterpreter :: Interpreter a -> InterpreterState -> IO (Either ErrorMsg a, InterpreterState)
runInterpreter = runStateT . runExceptT . unInterpreter

interpret :: [Stmt Expr] -> IO ()
interpret = void . interpret' initialInterpreterState

interpret' :: InterpreterState -> [Stmt Expr] -> IO InterpreterState
interpret' startState stmts = do
  (result, finalState) <-
    runInterpreter
      (traverse_ execute stmts)
      startState
  case result of
    Left e -> case e of 
      ErrorMsg err -> error $ T.unpack err
      Goto -> error "Unexpected Goto error."
    Right () -> return finalState

interpretExpr :: Expr -> IO (Either ErrorMsg Expr)
interpretExpr expr = do
  (result, _finalState) <-
    runInterpreter (eval expr)
    initialInterpreterState
  return result

writeOut :: Text -> Interpreter ()
writeOut str = do
  collectOutputOn <- gets collectOutput
  -- If debug is enabled, log the output to the state
  if collectOutputOn
    then
      modify
          (\s -> s{output = str : output s})
    else do 
      liftIO $ putStr str
      liftIO $ hFlush stdout

writeOutLn :: Text -> Interpreter ()
writeOutLn str = writeOut $ str <> "\n"

execute :: Stmt Expr -> Interpreter ()
execute stmt = do
  debugOn <- gets debug
  when debugOn (liftIO $ print stmt)
  case stmt of
    StmtPrint expr -> do
      result <- eval expr
      writeOutLn $ stringify result
    StmtVarDecl (Var name) expr -> do
      result <- eval expr
      defVar (Var name) result
    StmtExpr expr -> do
      _ <- eval expr
      return ()
    StmtBlock stmts -> do
      pushEnv
      traverse_ execute stmts
      popEnv
    StmtIf cond tBody fBodyOpt -> do
      result <- eval cond
      if isTruthy result
        then execute tBody
        else traverse_ execute fBodyOpt
      return ()
    StmtWhile cond body ->
      whileM_ (fmap isTruthy . eval $ cond) (execute body)
    StmtFuncDecl name params body -> do
      -- Retrieve current environment stack
      gEnv <- gets globalEnv
      lEnvs <- gets localEnvs
      -- Create function object with environment stack
      let func =
            Func
              { params = params
              , body = body
              , funcGEnv = gEnv
              , funcLEnvs = lEnvs
              }
      -- Add reference to function in current environment
      defVar name (ExprFunc func)
    StmtReturn result -> do
      result' <- eval result
      modify (\s -> s{returnVal = result'})
      throwError Goto

pushEnv :: Interpreter ()
pushEnv = modify (\s -> s{localEnvs = LocalEnv HMap.empty : localEnvs s})

popEnv :: Interpreter ()
popEnv = do
  lEnvs <- gets localEnvs
  case lEnvs of
    [] -> addError $ ErrorMsg "Attempted to pop local environment block when none are on the stack."
    _headEnv : restEnvs ->
      modify (\s -> s{localEnvs = restEnvs})

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
defVar var expr = do
  exprRef <- liftIO $ newIORef expr
  modify
    ( \s ->
        case s of
          -- Handle the case where there is an active
          -- local environment.
          InterpreterState{localEnvs = (LocalEnv m) : lEnvs} ->
            s
              { localEnvs = LocalEnv (HMap.insert var exprRef m) : lEnvs
              }
          -- Handle the case where there is no active
          -- local environment.
          InterpreterState
            { globalEnv = GlobalEnv m
            , localEnvs = []
            } ->
              s
                { globalEnv = GlobalEnv (HMap.insert var exprRef m)
                }
    )

-- | Assign a new value to an existing variable. If the variable does not exist,
-- then the assignment shall _not_ define a new variable with the name.
assignVar :: Var -> Expr -> Interpreter ()
assignVar var@(Var vName) val = do
  gEnv <- gets globalEnv
  lEnvs <- gets localEnvs
  assignedLocal <- assignLocalVar lEnvs var val
  if assignedLocal then
    return ()
  else do
    assignedGlobal <- assignGlobalVar gEnv var val
    unless assignedGlobal
      (throwError . ErrorMsg $ "No variable (" <> vName <> ") found for assignment.")

assignLocalVar :: [LocalEnv] -> Var -> Expr -> Interpreter Bool
assignLocalVar envs var val =
  case remEnvs of
    (LocalEnv m) : _xs -> do
      case HMap.lookup var m of
        Just ref -> do 
          liftIO $ writeIORef ref val
          return True
        Nothing -> throwError $ ErrorMsg "Unexpected missing variable reference."
    _ -> return False
    where
      (_preEnvs, remEnvs) = break (HMap.member var . lMapping) envs

assignGlobalVar :: GlobalEnv -> Var -> Expr -> Interpreter Bool
assignGlobalVar env var val =
  case HMap.lookup var (gMapping env) of
    Just ref -> do
      liftIO $ writeIORef ref val
      return True
    Nothing -> return False

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
      mVal <- lookupVar gEnv lEnvs var
      case mVal of
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
    ExprFunc _fn -> return expr
    ExprCall target args -> do
      -- Target must evaluate to an `ExprFunc`
      target' <- eval target
      case target' of
        ExprFunc (Func fParams fBody fGEnv fLEnvs) -> do
          args' <- traverse eval args
          
          -- Check if too many arguments provided
          when (length args' > length fParams)
            (throwError . ErrorMsg $ "Too many arguments provided.")
          
          -- MBR: Looking closer, does Lox spec actually include currying?
          -- Check if we have enough arguments
          -- Return a new ExprFunc with fewer parameters and appropriate
          -- environment for previously provided arguments
          when (length args' < length fParams) undefined

          -- Define function paramaters in terms of args
          argsRefs <- traverse (liftIO . newIORef) args'
          let callEnv = LocalEnv $ HMap.fromList $ zip fParams argsRefs

          -- We want to restore the environment after the function call
          -- Note that since values are represented as IORef cells,
          -- mutation of variables in the saved environments will
          -- still occur
          progGEnv <- gets globalEnv
          progLEnvs <- gets localEnvs

          -- Install function environment
          modify
            ( \s ->
                s
                  { localEnvs = callEnv : fLEnvs
                  , globalEnv = fGEnv
                  }
            )

          -- Execute statement block associated with function
          catchError (execute fBody) 
            (\e -> case e of
              Goto -> return ()
              -- Rethrow the error if it isn't related to control flow
              _ -> throwError e)

          -- Get function result
          retVal <- gets returnVal
          -- Reinstate program environment
          modify
            ( \s ->
                s
                  { globalEnv = progGEnv
                  , localEnvs = progLEnvs
                  }
            )

          return retVal
        _ -> throwError . ErrorMsg $ ("Invalid call target: " <> stringify target' <> ".")



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
      case x' of
        ExprLit (LitNum _)-> do
          xNum <- getNum x'
          yNum <- getNum y'
          return $ ExprLit (LitNum $ xNum + yNum)
        ExprLit (LitStr _) -> do  
          xStr <- getStr x'
          yStr <- getStr y'
          return $ ExprLit (LitStr $ xStr <> yStr)
        _ -> throwError $ mkErrorMsg (stringify x') "number or string"
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

-- | This function determines if two Lox expressions
-- are comparable for the BinEq and BinNeq operations.
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

-- | When a number is expected, either throw 
-- an error message when a number is not found
-- or return the number.
getNum :: Expr -> Interpreter Float
getNum (ExprLit (LitNum x)) = return x
getNum expr =
  throwError . ErrorMsg $
    "Number expected but found: " <> stringify expr <> "."

-- | When a string is expected, either throw 
-- an error message that a string is not found
-- or return the string.
getStr :: Expr -> Interpreter Text
getStr (ExprLit (LitStr s)) = return s
getStr expr =
  throwError . ErrorMsg $
    "String expected but found: " <> stringify expr <> "."

getBool :: Expr -> Interpreter Bool
getBool (ExprLit (LitBool x)) = return x
getBool expr = return $ isTruthy expr

-- | Cast a non-boolean Lox expression to a boolean for use
-- in boolean operations. 
-- The `nil` value is `false`, all other expressions are
-- considered `true`. Boolean values are either true or false
-- depending on their value.
isTruthy :: Expr -> Bool
isTruthy (ExprLit LitNil) = False
isTruthy (ExprLit (LitBool x)) = x
isTruthy _ = True