module Bli.AstSpec where

import Bli.Ast
import Bli.Parse

import Data.Text (Text)

import Bli.Analysis (parseExpr)
import Test.HUnit (assertFailure)
import Test.Hspec
import Text.Megaparsec (parse)

assertParseFailure :: IO a
assertParseFailure = assertFailure "could not parse test expression"

exprShouldBe :: (Show a, Eq a) => Text -> (Expr -> IO a) -> a -> Expectation
exprShouldBe exprStr f expected = do
  expr <- parseTestExpr exprStr
  result <- f expr
  result `shouldBe` expected

parseTestExpr :: Text -> IO Expr
parseTestExpr exprStr =
  case parse pExpr "" exprStr of
    Right expr -> do
      case parseExpr expr of
        Right expr' -> do
          return expr'
        Left _ -> assertParseFailure
    Left _ -> assertParseFailure

spec :: Spec
spec = do
  describe "simple expressions" $ do
    it "should produce a number" $ do
      expr <- parseTestExpr "1"
      expr `shouldBe` ExprLit (LitNum 1.0)

    it "should produce a boolean" $ do
      expr <- parseTestExpr "true"
      expr `shouldBe` ExprLit (LitBool True)

  describe "nested expressions" $ do
    it "should produce a nested arithmetic expression" $ do
      expr <- parseTestExpr "(1 + 20) * 4"
      expr
        `shouldBe` ( ExprBin
                      ( BinExpr
                          BinMul
                          ( ExprGroup
                              ( ExprBin
                                  ( BinExpr
                                      BinAdd
                                      (ExprLit (LitNum 1.0))
                                      (ExprLit (LitNum 20.0))
                                  )
                              )
                          )
                          (ExprLit (LitNum 4.0))
                      )
                   )

    it "should produce a nested logic expression" $ do
      expr <- parseTestExpr "(32 > 19.32) and (false or (\"foo\" != \"bar\"))"
      expr
        `shouldBe` ( ExprBin
                      ( BinExpr
                          BinLogAnd
                          ( ExprGroup
                              ( ExprBin
                                  ( BinExpr
                                      BinGt
                                      (ExprLit (LitNum 32.0))
                                      (ExprLit (LitNum 19.32))
                                  )
                              )
                          )
                          ( ExprGroup
                              ( ExprBin
                                  ( BinExpr
                                      BinLogOr
                                      (ExprLit (LitBool False))
                                      ( ExprGroup
                                          ( ExprBin
                                              ( BinExpr
                                                  BinNeq
                                                  (ExprLit (LitStr "foo"))
                                                  (ExprLit (LitStr "bar"))
                                              )
                                          )
                                      )
                                  )
                              )
                          )
                      )
                   )

  describe "stringifying expressions" $ do
    it "should support numbers" $ do
      expr <- parseTestExpr "42"
      stringify expr `shouldBe` "42"
    it "should support nested arithmetic" $ do
      expr <- parseTestExpr "(44 - 2) * 42"
      stringify expr `shouldBe` "(44 - 2) * 42"
    it "should support logic expressions" $ do
      expr <- parseTestExpr "(true and false) or ((100.4 < 90.1234) and !(true and true))" 
      stringify expr `shouldBe` "(true and false) or ((100.4 < 90.1234) and !(true and true))"