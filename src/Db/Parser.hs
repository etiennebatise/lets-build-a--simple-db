{-# LANGUAGE OverloadedStrings #-}

module Db.Parser where

import           Data.String.Conversions
import qualified Data.Text.Format as Fmt
import           Text.Parsec (Parsec, parse, try, choice, token, many, many1, (<?>) )
import           Text.Parsec.Char (string, char, satisfy, spaces, digit, alphaNum, letter)
import           Text.Parsec.Error
import           Text.Parsec.String (Parser)

import           Db.Db (Row(..))

data MetaCommand = Exit
  deriving (Read)

data MetaCommandParserError = UnrecognizedCommand String

instance Show MetaCommandParserError where
  show (UnrecognizedCommand i) = convertString
                                 $ Fmt.format "Unrecognized command '{}'.\n"
                                 (Fmt.Only i)

metaCommands :: [(String, MetaCommand)]
metaCommands =
  [ (".exit", Exit) ]

metaCommandParser :: Parser MetaCommand
metaCommandParser = choice $ fmap (\(fst, snd) -> try $ snd <$ string fst) metaCommands
-- metaCommandParser = choice $ fmap (try . uncurry ($>) . first string)  metaCommands

metaParser :: Parser Char
metaParser = char '.'

data Statement = Select | Insert Row

data StatementParserError = UnrecognizedStatement String
                         | SyntaxError

instance Show StatementParserError where
  show (UnrecognizedStatement i) = convertString
                                   $ Fmt.format "Unrecognized keyword at start of '{}'."
                                   (Fmt.Only i)
  show SyntaxError = "Syntax error. Could not parse statement."

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