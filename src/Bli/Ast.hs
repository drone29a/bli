module Bli.Ast where

import Data.Text (Text)
import qualified Data.Text as T

data Literal
  = LitNum Float
  | LitStr Text
  | LitBool Bool
  | LitNil
  deriving (Eq, Show)

data Expr
  = ExprLit Literal
  | ExprVar Text
  | ExprUn UnOp Expr
  | ExprBin BinOp Expr Expr
  | ExprGroup Expr
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

newtype ErrorMsg = ErrorMsg Text
  deriving (Eq, Show)

mkErrorMsg :: Show a => a -> a -> ErrorMsg
mkErrorMsg found expected =
  ErrorMsg . T.pack $ "Found " <> show found <> ", but expected " <> show expected <> "."

-- | Recursively evaluate an expression.
eval :: Expr -> Either ErrorMsg Expr
eval expr = do
  case expr of
    ExprLit _lit -> Right expr
    ExprUn op x -> evalUnary op x
    ExprBin op x y -> evalBinary op x y
    ExprGroup x -> eval x
    ExprVar _ -> Right $ ExprLit LitNil

evalUnary :: UnOp -> Expr -> Either ErrorMsg Expr
evalUnary UnNeg x = do
  result <- eval x
  case result of
    ExprLit (LitNum val) -> Right $ ExprLit (LitNum (-val))
    _ -> Left $ mkErrorMsg (stringify result) "number"
evalUnary UnNot x = do
  result <- eval x
  Right $ ExprLit (LitBool $ not . isTruthy $ result)

evalBinary :: BinOp -> Expr -> Expr -> Either ErrorMsg Expr
evalBinary op x y = do
  x' <- eval x
  y' <- eval y

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

stringify :: Expr -> Text
stringify (ExprLit LitNil) = "nil"
stringify (ExprLit (LitNum x)) =
  let str = T.pack $ show x in
    if T.isSuffixOf ".0" str then
      T.drop 2 str
    else
      str
stringify (ExprLit (LitBool x)) =
  if x then "true" else "false"
stringify (ExprLit (LitStr x)) = x
stringify expr = T.pack . show $ expr
