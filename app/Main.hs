{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Data.Bifunctor (first, second)
import Data.Either (either)
import Data.Functor (($>))
import Data.IORef
import Data.List (lookup)
import Data.Tuple (uncurry, swap)
import Data.Word
import System.IO
import Text.Parsec.Error (errorMessages, messageString)

import Db.Db
import Db.Parser
import Db.Exec

main :: IO ()
main = do
  initTable <- newIORef newTable
  forever $ do
    printPrompt
    input <- getLine
    either
      (const $ handleStatement input initTable)
      (const $ handleMeta input)
      $ parse' metaParser input

handleMeta :: String -> IO ()
handleMeta i = case parse' metaCommandParser i of
  Left e -> print $ UnrecognizedCommand i
  Right c -> executeMetaCommand c

handleStatement :: String -> IORef Table -> IO ()
handleStatement i tableRef =
    case parse' statementParser i of
      -- Left e -> putStrLn $ messageString $ last $ errorMessages e
      Left e -> print e
      Right s -> handleStatementOutcome =<< executeStatement s =<< readIORef tableRef
  where
    handleStatementOutcome :: Either ExecutionError Table -> IO ()
    handleStatementOutcome (Left e) = print e
    handleStatementOutcome (Right t) = do
                                writeIORef tableRef t
                                putStrLn "Executed."


printPrompt :: IO ()
printPrompt = do
  putStr "db > "
  hFlush stdout
