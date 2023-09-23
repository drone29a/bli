{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Bli.InterpreterSpec where

import Bli.Analysis (parseExpr)
import Bli.Ast
import Bli.Error (ErrorMsg (ErrorMsg))
import Bli.Interpreter
import Bli.Parse (pExpr, pProg)

import qualified Data.HashMap.Strict as HMap
import Data.Text (unlines, Text)
import Test.HUnit
import Test.Hspec
import Text.Megaparsec (parse)
import Prelude hiding (unlines)
import qualified Data.Text as T

assertParseFailure :: IO a
assertParseFailure = assertFailure "could not parse test expression"

assertExecutionFailure :: IO a
assertExecutionFailure = assertFailure "unexpected interpreter state"

testInterpret :: Text -> IO InterpreterState
testInterpret prog = 
  case parse pProg "" prog of
    Right stmts -> do
      interpret' initialInterpreterState{debug=False} (process stmts)
    Left _ -> assertParseFailure

exprShouldBe :: (Show a, Eq a) => Text -> (Expr -> IO a) -> a -> Expectation
exprShouldBe exprStr f expected =
  case parse pExpr "" exprStr of
    Right expr -> do
      case parseExpr expr of
        Right expr' -> do
          result <- f expr'
          result `shouldBe` expected
        Left _ -> assertParseFailure
    Left _ -> assertParseFailure

shouldOutput :: Text -> [Text] -> Expectation
shouldOutput prog expected = do
    InterpreterState{output} <- testInterpret prog
    reverse output `shouldBe` expected

shouldHaveError :: Text -> ErrorMsg -> Expectation
shouldHaveError prog errorMsg = do
    InterpreterState{Bli.Interpreter.errors=errorMsgs} <- testInterpret prog
    errorMsgs `shouldContain` [errorMsg]

stateShould :: Text -> (InterpreterState -> Expectation) -> Expectation
stateShould prog checkState = do
  state <- testInterpret prog
  checkState state

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

  describe "print statements" $ do
    it "should print values" $ do
      shouldOutput "print \"hello\";" ["\"hello\"\n"]

  describe "variable declaration, assignment, and scope" $ do
    it "should support global variable declaration" $ do
      "var x = 3;"
        `stateShould`
        ( \s ->
            (HMap.toList . gMapping . globalEnv $ s) `shouldContain` [(Var "x", ExprLit . LitNum $ 3)]
        )

    it "should support redefining a global variable" $ do
      unlines 
        [ "var x = 1;"
        , "var x = 10;"
        ]
        `stateShould`
        ( \s ->
          (HMap.toList . gMapping . globalEnv $ s) `shouldContain` [(Var "x", ExprLit . LitNum $ 10)]
        )
    
    it "should support local variable declarations" $ do
      unlines
        [ "{"
        , "  var outer = \"abc\";"
        , "  {"
        , "    var inner = 30;"
        , "    print inner;"
        , "    print outer;"
        , "  }"
        , "}"
        ]
        `shouldOutput`
        ["30\n", "\"abc\"\n"]

    it "should support an environment with global and nested local blocks" $ do
      prog <- T.pack <$> readFile "res/ch8_scope_test.lox"
      prog `shouldOutput` 
        [ "\"inner a\"\n"
        , "\"outer b\"\n"
        , "\"global c\"\n"
        , "\"outer a\"\n"
        , "\"outer b\"\n"
        , "\"global c\"\n"
        , "\"global a\"\n"
        , "\"global b\"\n"
        , "\"global c\"\n"]

    
    it "should error when variable is not in scope" $ do
      let prog = unlines [ "{"
                         , "  var outer = \"abc\";"
                         , "  {"
                         , "    var inner = 30;"
                         , "  }"
                         , "  print inner;"
                         , "}"
                         ]
      testInterpret prog `shouldThrow` errorCall "No variable named \'inner\' found."

    it "should initialize a variable without a definition to nil" $ do
      unlines
        [ "var a;"
        , "print a;"
        ]
        `shouldOutput`
        ["nil\n"]
