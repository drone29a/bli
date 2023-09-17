module Main where

import Bli.Interpreter (interpret, process)
import Bli.Parse (pProg)

import Prelude hiding (readFile)

import Data.Text.IO (readFile)
import Text.Megaparsec (parse)
import System.Environment (getArgs)

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
      interpret stmts'
    Left err -> do
      print err

main :: IO ()
main = do
  args <- getArgs
  let sourcePath = head args
  execute sourcePath

