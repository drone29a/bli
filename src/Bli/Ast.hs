{-# OPTIONS_GHC -Wno-orphans #-}
module Bli.Ast where

import Data.Text (Text, intercalate)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.IORef (IORef, readIORef)
import qualified Data.HashMap.Strict as HMap

instance Show (IORef a) where
    show _ = "<ioref>"

type Mapping = HashMap Var (IORef Expr)

newtype GlobalEnv = GlobalEnv {gMapping :: Mapping}
  deriving (Eq, Show, Generic)

newtype LocalEnv = LocalEnv {lMapping :: Mapping}
  deriving (Eq, Show, Generic)

-- | Remove IORef wrappers for `Expr` values
-- in a `Mapping`. Useful for testing and debugging.
dereference :: Mapping -> IO (HashMap Var Expr)
dereference m = do
  let kvs = HMap.toList m
  HMap.fromList <$> traverse unwrapRef kvs
    where 
      unwrapRef :: (Var, IORef Expr) -> IO (Var, Expr)
      unwrapRef = traverse readIORef

data Func a = Func 
  { params :: [Var]
  , body :: Stmt a
  , funcGEnv :: GlobalEnv
  , funcLEnvs :: [LocalEnv]
  } deriving (Eq, Show, Generic)

data Literal
  = LitNum Float
  | LitStr Text
  | LitBool Bool
  | LitNil
  deriving (Eq, Show, Generic)
instance Hashable Literal

newtype Var = Var Text
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

newtype Ident = Ident Text
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

data BinExpr a = BinExpr BinOp a a
  deriving
    ( Eq
    , Show
    , Functor
    , Foldable
    , Traversable
    , Generic
    , Hashable
    )

data UnExpr a = UnExpr UnOp a
  deriving
    ( Eq
    , Show
    , Functor
    , Foldable
    , Traversable
    , Generic
    , Hashable
    )

data Asgn a b = Asgn a b
  deriving (Eq, Show, Functor, Generic, Hashable)

newtype LVal = LValVar Var
  deriving (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- | The `PExpr` type is used by `Bli.Parse` to construct
-- an initial Lox AST according to the capabilities of the parser.
data PExpr 
  = PExprLit Literal
  | PExprVar Var
  | PExprUn (UnExpr PExpr)
  | PExprBin (BinExpr PExpr)
  | PExprGroup PExpr
  | PExprAsgn (Asgn LVal PExpr)
  | PExprCall PExpr [PExpr]
  deriving (Eq, Show, Generic, Hashable)

-- | The `Expr` type is used for a final representation
-- of Lox code. If additional post-procesing after parsing
-- is required, it can be implemented as a translation
-- between `PExpr` and `Expr`.
data Expr
  = ExprLit Literal
  | ExprVar Var
  | ExprUn (UnExpr Expr)
  | ExprBin (BinExpr Expr)
  | ExprGroup Expr
  | ExprAsgn (Asgn LVal Expr)
  | ExprCall Expr [Expr]
  | ExprFunc (Func Expr)
  deriving (Eq, Show, Generic)

data UnOp
  = UnNeg
  | UnNot
  deriving (Eq, Show, Generic, Hashable)

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
  deriving (Eq, Show, Generic, Hashable)

data Stmt a
  = StmtExpr a
  | StmtVarDecl Var a
  | StmtPrint a
  | StmtBlock [Stmt a]
  | StmtIf a (Stmt a) (Maybe (Stmt a))
  | StmtWhile a (Stmt a)
  | StmtFuncDecl Var [Var] (Stmt a)
  | StmtReturn a
  deriving
    ( Eq
    , Show
    , Functor
    , Foldable
    , Traversable
    , Generic
    , Hashable
    )

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
stringify (ExprUn (UnExpr op x)) = unOpSym op <> stringify x
stringify (ExprBin (BinExpr op x y)) = stringify x <> " " <> binOpSym op <> " " <> stringify y
stringify (ExprVar (Var name)) = name
stringify (ExprAsgn (Asgn (LValVar (Var name)) expr)) = name <> " = " <> stringify expr
stringify (ExprCall tgt args) = 
  stringify tgt <> "(" <> intercalate "," (fmap stringify args) <> ")"
stringify (ExprFunc _) = "<fn>"

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
