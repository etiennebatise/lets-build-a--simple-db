{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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
import System.Exit
import System.IO

import Db.Parser
import Db.Db

data ExecutionError = TableFull

instance Show ExecutionError where
  show TableFull = "Error: Table full."

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
    case parse' statementTypeParser i of
      Left e -> print $ UnrecognizedStatement i
      Right _ -> case parse' statementParser i of
        Left e -> print SyntaxError
        Right c -> handleStatementOutcome =<< executeStatement c =<< readIORef tableRef
  where
    handleStatementOutcome :: Either ExecutionError Table -> IO ()
    handleStatementOutcome (Left e) = print e
    handleStatementOutcome (Right t) = do
                                writeIORef tableRef t
                                putStrLn "Executed."

executeMetaCommand :: MetaCommand -> IO ()
executeMetaCommand = \case
  Exit -> exitSuccess

executeStatement :: Statement -> Table -> IO (Either ExecutionError Table)
executeStatement s t = go
  where
    go = case s of
      Insert r -> executeInsert t r
      Select ->  executeSelect t


executeInsert :: Table -> Row -> IO (Either ExecutionError Table)
executeInsert table row = if numberOfRows table >= tableMaxRows
                         then pure $ Left TableFull
                         else pure $ Right $ writeSlot table (numberOfRows table) row

executeSelect :: Table -> IO (Either ExecutionError Table)
executeSelect table = do
    let range = [0..(numberOfRows table - 1)]
    putStr $ unlines $ fmap (show . rowToTuple . readSlot table) range
    pure $ Right table

rowToTuple :: Row -> (UserId, Username, Email)
rowToTuple r = (rid r, name r, email r)

printPrompt :: IO ()
printPrompt = do
  putStr "db > "
  hFlush stdout
