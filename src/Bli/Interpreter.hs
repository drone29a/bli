module Bli.Interpreter where

import Bli.Ast (Expr, eval, stringify)

interpret :: Expr -> IO ()
interpret expr = do
  case eval expr of
    Right result -> print $ stringify result
    Left errorMsg -> print errorMsg
