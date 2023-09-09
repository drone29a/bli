module Bli.InterpreterSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "evaluating expressions" $ do
    it "should support arithmetic expressions" $ do
      exprShouldBe "(1 + 10.5) * 50.5 / 2" eval (Right (ExprLit (LitNum 290.375)))
    it "should support boolean expressions" $ do
      exprShouldBe "true and (false or (1 <= 2))" eval (Right (ExprLit (LitBool True)))
    it "should should produce type errors" $ do
      exprShouldBe "42.5 + true" eval (Left (ErrorMsg "Number expected but found: true."))
    it "should correctly implement truthiness for nil" $ do
      exprShouldBe "nil and true" eval (Right (ExprLit (LitBool False)))
    it "should correctly implement truthiness for numbers and strings" $ do
      exprShouldBe "1 and \"foo\" and true" eval (Right (ExprLit (LitBool True)))