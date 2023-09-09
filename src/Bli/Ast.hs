module Bli.Ast where

import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HMap
import Data.Either.Combinators (maybeToRight)
import GHC.Generics (Generic)
import Data.Hashable (Hashable)

newtype Environment = Environment
  { mapping :: HashMap Var Expr
  } deriving (Eq, Show)

lookupVar :: Environment -> Var -> Maybe Expr
lookupVar (Environment mapping) var =
  HMap.lookup var mapping

data Literal
  = LitNum Float
  | LitStr Text
  | LitBool Bool
  | LitNil
  deriving (Eq, Show)

newtype Var = Var Text
  deriving (Eq, Show, Generic)

instance Hashable Var

data Expr
  = ExprLit Literal
  | ExprVar Var
  | ExprUn UnOp Expr
  | ExprBin BinOp Expr Expr
  | ExprGroup Expr
  | ExprAsgn Asgn
  deriving (Eq, Show)

data Asgn = Asgn Var Expr
  deriving (Eq, Show)

data UnOp
  = UnNeg
  | UnNot
  deriving (Eq, Show)

data BinOp
  = BinEq
  | BinNeq
  | BinLt
  | BinLte
  | BinGt
  | BinGte
  | BinAdd
  | BinSub
  | BinMul
  | BinDiv
  | BinLogAnd
  | BinLogOr
  deriving (Eq, Show)

data Stmt
  = StmtExpr Expr
  | StmtDecl Var Expr
  | StmtPrint Expr
  deriving (Eq, Show)

newtype ErrorMsg = ErrorMsg Text
  deriving (Eq, Show)

mkErrorMsg :: Show a => a -> a -> ErrorMsg
mkErrorMsg found expected =
  ErrorMsg . T.pack $ "Found " <> show found <> ", but expected " <> show expected <> "."

-- | Recursively evaluate an expression.
eval :: Environment -> Expr -> Either ErrorMsg Expr
eval env expr = do
  case expr of
    ExprLit _lit -> Right expr
    ExprUn op x -> evalUnary env op x
    ExprBin op x y -> evalBinary env op x y
    ExprGroup x -> eval env x
    ExprVar var@(Var name) ->
      maybeToRight
        ( ErrorMsg $
            "No variable named \'"
              <> name
              <> "\' found."
        )
        $ lookupVar env var
    ExprAsgn (Asgn var@(Var name) expr) ->
      undefined

evalUnary :: Environment -> UnOp -> Expr -> Either ErrorMsg Expr
evalUnary env UnNeg x = do
  result <- eval env x
  case result of
    ExprLit (LitNum val) -> Right $ ExprLit (LitNum (-val))
    _ -> Left $ mkErrorMsg (stringify result) "number"
evalUnary env UnNot x = do
  result <- eval env x
  Right $ ExprLit (LitBool $ not . isTruthy $ result)

evalBinary :: Environment -> BinOp -> Expr -> Expr -> Either ErrorMsg Expr
evalBinary env op x y = do
  x' <- eval env x
  y' <- eval env y

  case op of
    BinEq -> do
      checkEq x' y'
      Right $ ExprLit (LitBool $ x' == y')
    BinNeq -> do
      checkEq x' y'
      Right $ ExprLit (LitBool $ x' /= y')
    BinLt -> do
      xNum <- getNum x'
      yNum <- getNum y'
      Right $ ExprLit (LitBool $ xNum < yNum)
    BinLte -> do
      xNum <- getNum x'
      yNum <- getNum y'
      Right $ ExprLit (LitBool $ xNum <= yNum)
    BinGt -> do
      xNum <- getNum x'
      yNum <- getNum y'
      Right $ ExprLit (LitBool $ xNum > yNum)
    BinGte -> do
      xNum <- getNum x'
      yNum <- getNum y'
      Right $ ExprLit (LitBool $ xNum >= yNum)
    BinAdd -> do
      xNum <- getNum x'
      yNum <- getNum y'
      Right $ ExprLit (LitNum $ xNum + yNum)
    BinSub -> do
      xNum <- getNum x'
      yNum <- getNum y'
      Right $ ExprLit (LitNum $ xNum - yNum)
    BinMul -> do
      xNum <- getNum x'
      yNum <- getNum y'
      Right $ ExprLit (LitNum $ xNum * yNum)
    BinDiv -> do
      xNum <- getNum x'
      yNum <- getNum y'
      Right $ ExprLit (LitNum $ xNum / yNum)
    BinLogAnd -> do
      xBool <- getBool x'
      yBool <- getBool y'
      Right $ ExprLit (LitBool $ xBool && yBool)
    BinLogOr -> do
      xBool <- getBool x'
      yBool <- getBool y'
      Right $ ExprLit (LitBool $ xBool || yBool)

checkEq :: Expr -> Expr -> Either ErrorMsg ()
checkEq x y =
  case (x, y) of
    (ExprLit (LitNum _), ExprLit (LitNum _)) -> Right ()
    (ExprLit (LitStr _), ExprLit (LitStr _)) -> Right ()
    (ExprLit (LitBool _), ExprLit (LitBool _)) -> Right ()
    (ExprLit LitNil, _) -> Right ()
    (_, ExprLit LitNil) -> Right ()
    _ ->
      Left . ErrorMsg $
        "Operands: " <> stringify x <> " and " <> stringify y <> "are not comparable."

getNum :: Expr -> Either ErrorMsg Float
getNum (ExprLit (LitNum x)) = Right x
getNum expr =
  Left . ErrorMsg $
    "Number expected but found: " <> stringify expr <> "."

getBool :: Expr -> Either ErrorMsg Bool
getBool (ExprLit (LitBool x)) = Right x
getBool expr =
  Left . ErrorMsg $
    "Boolean expected but found: " <> stringify expr <> "."

isTruthy :: Expr -> Bool
isTruthy (ExprLit LitNil) = False
isTruthy (ExprLit (LitBool x)) = x
isTruthy _ = True

{- | The `stringify` function should take an `Expr`
and generate a `Text` string that resembles Lox syntax.

For example, an expression such as:
ExprUn UnNeg 
  (ExprGroup 
    (ExprBin BinAdd 
             (ExprLit LitNum 1)
             (Expr
             Lit LitNum 2.4)))
Would be represented as the string:
"-(1 + 2.4)"
-}
stringify :: Expr -> Text
stringify (ExprLit LitNil) = "nil"
stringify (ExprLit (LitNum x)) =
  let str = T.pack $ show x in
    if T.isSuffixOf ".0" str then
      T.dropEnd 2 str
    else
      str
stringify (ExprLit (LitBool x)) =
  if x then "true" else "false"
stringify (ExprLit (LitStr x)) = "\"" <> x <> "\""
stringify (ExprGroup x) = "(" <> stringify x <> ")"
stringify (ExprUn op x) = unOpSym op <> stringify x
stringify (ExprBin op x y) = stringify x <> " " <> binOpSym op <> " " <> stringify y
stringify (ExprVar (Var name)) = name
stringify (ExprAsgn (Asgn (Var name) expr)) = name <> " = " <> stringify expr

-- | Returns the Lox string symbol that corresponds to the unary operator.
unOpSym :: UnOp -> Text
unOpSym op = 
  case op of
    UnNeg -> "-"
    UnNot -> "!"

-- | Returns the Lox string symbol that corresponds to the binary operator.
binOpSym :: BinOp -> Text
binOpSym op =
  case op of
    BinAdd -> "+"
    BinSub -> "-"
    BinMul -> "*"
    BinDiv -> "/"
    BinLt -> "<"
    BinLte -> "<="
    BinGt -> ">"
    BinGte -> ">="
    BinEq -> "=="
    BinNeq -> "!="
    BinLogAnd -> "and"
    BinLogOr -> "or"
