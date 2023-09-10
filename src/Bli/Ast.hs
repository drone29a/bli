module Bli.Ast where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Hashable (Hashable)

data Literal
  = LitNum Float
  | LitStr Text
  | LitBool Bool
  | LitNil
  deriving (Eq, Show)

newtype Var = Var Text
  deriving (Eq, Show, Generic)

instance Hashable Var

-- MBR: Consider distinguishing between parsed expressions
--      and analyzed expressions. Perhaps with PExpr vs Expr
data Expr
  = ExprLit Literal
  | ExprVar Var
  | ExprUn UnOp Expr
  | ExprBin BinOp Expr Expr
  | ExprGroup Expr
  | ExprAsgn Asgn
  | ExprPossAsgn PossAsgn
  deriving (Eq, Show)

newtype LVal = LValVar Var
  deriving (Eq, Show)

data Asgn = Asgn LVal Expr
  deriving (Eq, Show)

data PossAsgn = PossAsgn Expr Expr
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
stringify (ExprAsgn (Asgn (LValVar (Var name)) expr)) = name <> " = " <> stringify expr
stringify (ExprPossAsgn (PossAsgn left right)) = stringify left <> " = " <> stringify right

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
