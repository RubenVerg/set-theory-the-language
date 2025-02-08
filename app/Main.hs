module Main where

import Language.STTL.Set
import Language.STTL.Parser
import Language.STTL.Interpreter
import Language.STTL.Context
import Language.STTL.Multiverse
import Language.STTL.DefaultUniverses

import System.IO
import System.Environment
import System.Exit
import Data.List

readWithFlags :: [String] -> String -> Context Set
readWithFlags flags str = let
  input = (!! 1) <$> find ((== 'i') . head) flags
  in case input of
    Nothing -> parseExpr "<input>" str >>= interpretExpr
    Just u -> case lookup u universes of
      Nothing -> throwError $ "Unknown universe: " ++ [u]
      Just un -> case universeRead un of
        Nothing -> throwError $ "Cannot parse in universe: " ++ [u]
        Just f -> f str

showWithFlags :: [String] -> Set -> Context String
showWithFlags flags set = let
  output = (!! 1) <$> find ((== 'o') . head) flags
  in case output of
    Nothing -> pure $ show set
    Just u -> case lookup u universes of
      Nothing -> throwError $ "Unknown universe: " ++ [u]
      Just un -> case universeShow un of
        Nothing -> throwError $ "Cannot show in universe: " ++ [u]
        Just f -> f set

repl :: [String] -> IO ()
repl flags = let
  go = do
    putStr "> "
    hFlush stdout
    line <- getLine
    if null line || line == ":q" then return ()
    else runContext (run "<repl>" line) >>= (\case
      Left err -> putStrLn err
      Right Nothing -> return ()
      Right (Just v) -> runContext (showWithFlags flags v) >>= (\case
        Left err -> putStrLn err
        Right str -> putStrLn str)) >> go
  in putStrLn "Set Theory: The Language REPL; :q to exit." >> go

runFile :: [String] -> FilePath -> [String] -> IO ()
runFile flags file args = do
  inputs <- mapM (\arg -> runContext (readWithFlags flags arg) >>= (\case
    Left err -> putStrLn err *> exitFailure
    Right res -> pure res)) args
  contents <- readFile file
  let str = contents ++ " " ++ intercalate " " (show <$> inputs)
  runContext (run file str) >>= (\case
    Left err -> putStrLn err
    Right Nothing -> pure ()
    Right (Just v) -> runContext (showWithFlags flags v) >>= (\case
      Left err -> putStrLn err *> exitFailure
      Right r -> putStrLn r))

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  allArgs <- getArgs
  let posArgs = filter (not . isPrefixOf "-") allArgs
  let flags = allArgs >>= (\a -> if null a then [] else if head a == '-' then [tail a] else [])

  if null posArgs then repl flags
  else runFile flags (head posArgs) (tail posArgs)
