module Bli.Analysis where
import Bli.Ast (Expr (..), PExpr (..), Asgn (..), LVal (LValVar), Stmt)
import Data.Text ( Text )
import Data.Text qualified as T

-- | Attempt to produce a proper Expr from a PExpr
parseExpr :: PExpr -> Either Text Expr
parseExpr (PExprLit lit) = Right $ ExprLit lit
parseExpr (PExprVar var) = Right $ ExprVar var
parseExpr (PExprUn unExpr) = do
    ExprUn <$> traverse parseExpr unExpr
parseExpr (PExprBin binExpr) = do
    ExprBin <$> traverse parseExpr binExpr
parseExpr (PExprGroup expr) = do
    ExprGroup <$> parseExpr expr
parseExpr (PExprAsgn (Asgn left right)) = do
    -- left' <- parseLVal left
    right' <- parseExpr right
    Right . ExprAsgn $ Asgn left right'

-- | Parse an LVal from an expression
parseLVal :: PExpr -> Either Text LVal
parseLVal (PExprVar var) = Right $ LValVar var
parseLVal expr = Left $ "The expression \"" <> T.pack (show expr) <> "\" is not an l-value."

process :: [Stmt PExpr] -> [Stmt Expr]
process stmts =
  let result = traverse (traverse parseExpr) stmts in
    case result of
      Left _ -> []
      Right stmts' -> stmts'