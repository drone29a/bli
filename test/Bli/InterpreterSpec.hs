module Bli.InterpreterSpec where

import Bli.Ast
import Bli.Interpreter
import Bli.Parse (pExpr)

import Data.Text
import Test.HUnit
import Test.Hspec
import Text.Megaparsec (parse)
import Bli.Analysis (parseExpr)
import Bli.Error (ErrorMsg(ErrorMsg))

assertParseFailure :: IO a
assertParseFailure = assertFailure "could not parse test expression"

exprShouldBe :: (Show a, Eq a) => Text -> (Expr -> IO a) -> a -> Expectation
exprShouldBe exprStr f expected =
  case parse pExpr "" exprStr of
    Right expr -> do
        case parseExpr expr of
            Right expr' ->  do
                result <- f expr'
                result `shouldBe` expected
            Left _ -> assertParseFailure
    Left _ -> assertParseFailure

spec :: Spec
spec = do
  describe "evaluating expressions" $ do
    it "should support arithmetic expressions" $ do
      exprShouldBe "(1 + 10.5) * 50.5 / 2" interpretExpr (Right (ExprLit (LitNum 290.375)))
    it "should support boolean expressions" $ do
      exprShouldBe "true and (false or (1 <= 2))" interpretExpr (Right (ExprLit (LitBool True)))
    it "should should produce type errors" $ do
      exprShouldBe "42.5 + true" interpretExpr (Left (ErrorMsg "Number expected but found: true."))
    it "should correctly implement truthiness for nil" $ do
      exprShouldBe "nil and true" interpretExpr (Right (ExprLit (LitBool False)))
    it "should correctly implement truthiness for numbers and strings" $ do
      exprShouldBe "1 and \"foo\" and true" interpretExpr (Right (ExprLit (LitBool True)))