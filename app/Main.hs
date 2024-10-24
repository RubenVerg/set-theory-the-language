module Main where

import Language.STTL.Interpreter
import Language.STTL.Context

import System.IO

repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  line <- getLine
  if line == ":q" then return ()
  else runContext (run "<repl>" line) >>= putStrLn . (\case
    Left err -> err
    Right Nothing -> ""
    Right (Just v) -> show v) >> repl

main :: IO ()
main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  repl
