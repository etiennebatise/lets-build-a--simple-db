{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Debug.Trace

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Data.Binary.Get as Get
import Data.Binary.Put as Put
import Data.Binary.Builder as Bld
import Data.Bifunctor (first, second)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LB
import Data.Either (either)
import Data.Functor (($>))
import Data.Int (Int32, Int64)
import Data.IORef
import Data.List (lookup)
import Data.String.Conversions
import qualified Data.Text.Format as Fmt
import Data.Tuple (uncurry, swap)
import Data.Word
import Lib
import Text.Parsec (Parsec, parse, try, choice, token, many, many1, (<?>) )
import Text.Parsec.Char (string, char, satisfy, spaces, digit, alphaNum, letter)
import Text.Parsec.Error
import Text.Parsec.String (Parser)
import System.Exit
import System.IO


------------
-- PARSER --
------------

data MetaCommand = Exit
  deriving (Read)

metaCommands :: [(String, MetaCommand)]
metaCommands =
  [ (".exit", Exit) ]

metaCommandParser :: Parser MetaCommand
metaCommandParser = choice $ fmap (\(fst, snd) -> try $ snd <$ string fst) metaCommands
-- metaCommandParser = choice $ fmap (try . uncurry ($>) . first string)  metaCommands

metaParser :: Parser Char
metaParser = char '.'

data Statement = Select | Insert Row

statementTypeParser :: Parser String
statementTypeParser = choice
                      [ string "select"
                      , string "insert"]

selectParser :: Parser Statement
selectParser = do
  _ <- string "select"
  pure Select

insertParser :: Parser Statement
insertParser = do
  _ <- string "insert"
  _ <- spaces
  userId <- many1 digit
  _ <- spaces
  name <- many1 letter
  _ <- spaces
  email <- many1 alphaNum
  pure $ Insert $ Row (read userId) name email

statementParser :: Parser Statement
statementParser = choice [ selectParser, insertParser ]

parse' :: Parser a -> String -> Either ParseError a
parse' p = parse p ""

--------
-- DB --
--------

type UserId = Int64
type Username = String
type Email = String

type SizeInBytes = Int64

idSize ::SizeInBytes
idSize = 8

idOffset = 0

usernameSize ::SizeInBytes
usernameSize = 32

usernameOffset = idOffset + idSize;

emailSize ::SizeInBytes
emailSize = 255

emailOffset = usernameOffset + usernameSize;

rowSize = idSize + usernameSize + emailSize;

resize :: B.ByteString -> SizeInBytes -> B.ByteString
resize text size = B.append prefix text
  where
    prefix = B.pack $ Prelude.replicate (fromIntegral size - B.length text) 0

data Row = Row
  { rid :: UserId
  , name :: Username
  , email :: Email
  } deriving Show

class Serializable a where
  serialize :: a -> LB.ByteString
  deserialize :: LB.ByteString -> a

instance Serializable Row where
  serialize row = let i = Bld.putInt64be $ rid row
                      u = Bld.fromByteString $ resize (C.pack $ name row) usernameSize
                      e = Bld.fromByteString $ resize (C.pack $ email row) emailSize
                      total = Bld.append (Bld.append i u) e
                      result = Bld.toLazyByteString total
                  in result

  deserialize = runGet $ do
      index <- Get.getInt64be
      rem <- Get.getRemainingLazyByteString
      let name = C.unpack $
                 LB.toStrict $
                 LB.dropWhile (== 0) $
                 LB.take usernameSize rem
          email = C.unpack $
                  LB.toStrict $
                  LB.dropWhile (== 0) $
                  LB.take emailSize $
                  LB.drop usernameSize rem
      pure $ Row index name email

pageSize :: Int64
pageSize = 4096;

tableMaxPages :: Int64
tableMaxPages = 100;

rowsPerPage :: Int64
rowsPerPage = div pageSize rowSize;

tableMaxRows :: Int64
tableMaxRows = rowsPerPage * tableMaxPages;

data Table = Table
  { pages :: [LB.ByteString]
  , numberOfRows :: Int64
  } deriving (Show)

newTable :: Table
newTable = Table (Prelude.replicate (fromIntegral tableMaxPages) LB.empty) 0

readSlot :: Table -> Int64 -> Row
readSlot table rowNumber =
  let pageNumber = div rowNumber rowsPerPage
      page = pages table !! fromIntegral pageNumber
      rowOffset = mod rowNumber rowsPerPage
      row = LB.take rowSize $ LB.drop (rowOffset * rowSize) page
  in deserialize row

writeSlot :: Table -> Int64 -> Row -> Table
writeSlot table rowNumber row =
  let pageNumber = div rowNumber rowsPerPage
      (tablePrefix, page:tableSuffix) = splitAt (fromIntegral pageNumber) (pages table)
      rowOffset = mod rowNumber rowsPerPage
      prefix = LB.take (rowOffset  * rowSize) page
      suffix = LB.drop ((rowOffset + 1) * rowSize) page
      newPage = LB.concat [prefix, serialize row, suffix]
      newPages = concat [tablePrefix, [newPage], tableSuffix]
      newNumberOfRows = numberOfRows table + 1
  in Table newPages newNumberOfRows

---------------
-- EXECUTION --
---------------

data ExecutionError = TableFull


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
  where
    handleMeta i = case parse' metaCommandParser i of
      -- META_COMMAND_UNRECOGNIZED_COMMAND
      Left e -> Fmt.print "Unrecognized command '{}'\n" (Fmt.Only i)
      Right c -> executeMetaCommand c

    handleStatement i tableRef = case parse' statementTypeParser i of
      -- PREPARE_UNRECOGNIZED_STATEMENT
      Left e -> Fmt.print "Unrecognized keyword at start of '{}'\n" (Fmt.Only i)
      Right _ -> case parse' statementParser i of
        -- PREPARE_SYNTAX_ERROR
        Left e -> putStrLn "Syntax error. Could not parse statement" 
        Right c -> do
          table <- readIORef tableRef
          statementResult <- executeStatement table c
          case statementResult of
            Left TableFull -> putStrLn "Error: Table full."
            Right t -> do
              writeIORef tableRef t
              putStrLn "Success"


executeMetaCommand :: MetaCommand -> IO ()
executeMetaCommand = \case
  Exit -> exitSuccess

executeStatement :: Table -> Statement -> IO (Either ExecutionError Table)
executeStatement t s = go
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
    putStrLn $ unlines $ fmap (show . rowToTuple . readSlot table) range
    pure $ Right table

rowToTuple :: Row -> (UserId, Username, Email)
rowToTuple r = (rid r, name r, email r)

printPrompt :: IO ()
printPrompt = do
  putStr "db > "
  hFlush stdout
