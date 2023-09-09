module Bli.AstSpec where

import Bli.Ast
import Bli.Parse

import Data.Text (Text)

import Text.Megaparsec (parse)
import Test.HUnit (assertFailure)
import Test.Hspec

assertParseFailure :: IO a
assertParseFailure = assertFailure "could not parse test expression"

exprShouldBe :: (Show a, Eq a) => Text -> (Expr -> a) -> a -> Expectation
exprShouldBe exprStr f expected =
  case parse pExpr "" exprStr of
    Right expr -> do 
      f expr `shouldBe` expected
    Left _ -> assertParseFailure

spec :: Spec
spec = do
  describe "simple expressions" $ do
    it "should produce a number" $ do
      parse pExpr "" "1" `shouldBe` Right (ExprLit $ LitNum 1.0)

    it "should produce a boolean" $ do
      parse pExpr "" "true" `shouldBe` Right (ExprLit $ LitBool True)

  describe "nested expressions" $ do
    it "should produce a nested arithmetic expression" $ do
      parse pExpr "" "(1 + 20) * 4"
        `shouldBe` Right
          ( ExprBin
              BinMul
              ( ExprGroup
                  ( ExprBin
                      BinAdd
                      (ExprLit (LitNum 1.0))
                      (ExprLit (LitNum 20.0))
                  )
              )
              (ExprLit (LitNum 4.0))
          )

    it "should produce a nested logic expression" $ do
      parse pExpr "" "(32 > 19.32) and (false or (\"foo\" != \"bar\"))"
        `shouldBe` Right
          ( ExprBin
              BinLogAnd
              ( ExprGroup
                  ( ExprBin
                      BinGt
                      (ExprLit (LitNum 32.0))
                      (ExprLit (LitNum 19.32))
                  )
              )
              ( ExprGroup
                  ( ExprBin
                      BinLogOr
                      (ExprLit (LitBool False))
                      ( ExprGroup
                          ( ExprBin
                              BinNeq
                              (ExprLit (LitStr "foo"))
                              (ExprLit (LitStr "bar"))
                          )
                      )
                  )
              )
          )

  describe "stringifying expressions" $ do
    it "should support numbers" $ do
      case parse pExpr "" "42" of
        Right expr -> do
          stringify expr `shouldBe` "42"
        Left _ -> assertParseFailure
    it "should support nested arithmetic" $ do
      case parse pExpr "" "(44 - 2) * 42" of
        Right expr -> do
          stringify expr `shouldBe` "(44 - 2) * 42"
        Left _ -> assertParseFailure
    it "should support logic expressions" $ do
      case parse pExpr "" "(true and false) or ((100.4 < 90.1234) and !(true and true))" of
        Right expr -> do
          stringify expr `shouldBe` "(true and false) or ((100.4 < 90.1234) and !(true and true))"
        Left _ -> assertParseFailure
      