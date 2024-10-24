module Main where

import Language.STTL.Interpreter
import System.IO

repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  line <- getLine
  if line == ":q" then return ()
  else (putStrLn $ case run "<repl>" line of
    Left err -> err
    Right ast -> show ast) >> repl

main :: IO ()
main = do
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  repl
