module Main where

import Bli.Interpreter (
  InterpreterState (collectOutput),
  debug,
  initialInterpreterState,
  interpret',
  process,
 )
import Bli.Parse (pProg)

import Prelude hiding (readFile)

import Control.Monad (void)
import Data.Text.IO (readFile)
import System.Environment (getArgs)
import Text.Megaparsec (parse)

-- repl :: IO ()
-- repl = do
--   input <- T.pack <$> getLine
--   case input of
--     ":q" -> return ()
--     _ -> do
--       case parse pExpr "" input of
--         Right expr -> do
--           result <- interpret expr
--           print result
--           repl
--         Left err -> do
--           print err

execute :: FilePath -> IO ()
execute path = do
  input <- readFile path
  case parse pProg path input of
    Right stmts -> do
      let stmts' = process stmts
      void $
        interpret'
          initialInterpreterState
            { collectOutput = False
            , debug = False
            }
          stmts'
    Left err -> do
      print err

main :: IO ()
main = do
  args <- getArgs
  let sourcePath = head args
  execute sourcePath
