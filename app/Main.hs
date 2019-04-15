{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Data.Bifunctor (first, second)
import Data.Either (either)
import Data.Functor (($>))
import Data.List (lookup)
import Data.String.Conversions
import Data.Text
import qualified Data.Text.Format as Fmt
import Data.Tuple (uncurry, swap)
import Lib
import Text.Parsec (Parsec, parse, try, choice, token, many, many1)
import Text.Parsec.Char (string, char, satisfy, spaces, digit, alphaNum, letter)
import Text.Parsec.Error
import Text.Parsec.String (Parser)
import System.Exit
import System.IO

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

type UserId = Int
type Username = String
type Email = String

data Statement = Select | Insert UserId Username Email

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
  pure $ Insert (read userId) name email

statementParser :: Parser Statement
statementParser = choice [ selectParser, insertParser ]

parse' :: Parser a -> String -> Either ParseError a
parse' p = parse p ""

main :: IO ()
main = forever $ do
    printPrompt
    input <- getLine
    either
      (const $ handleStatement input)
      (const $ handleMeta input)
      $ parse' metaParser input
  where
    handleMeta i = case parse' metaCommandParser i of
      Left e -> Fmt.print "Unrecognized command '{}'\n" (Fmt.Only i)
      Right c -> executeMetaCommand c
    handleStatement i = case parse' statementParser i of
      Left e -> Fmt.print "Unrecognized keyword at start of '{}'\n" (Fmt.Only i)
      Right c -> executeStatement c


executeMetaCommand :: MetaCommand -> IO ()
executeMetaCommand = \case
  Exit -> exitSuccess

executeStatement :: Statement -> IO ()
executeStatement = \case
  Insert uid name email -> print "This is where we would do an insert.\n"
  Select -> print "This is where we would do an select.\n"

printPrompt :: IO ()
printPrompt = do
  putStr "db > "
  hFlush stdout
