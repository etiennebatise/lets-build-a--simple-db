{-# LANGUAGE OverloadedStrings #-}

module Db.Parser where

import          Control.Monad.Combinators (count')
import          Control.Monad (join)
import           Data.String.Conversions
import qualified Data.Text.Format as Fmt
import           Text.Parsec (Parsec, ParsecT, Stream, parse, try, choice, token, many, many1, (<?>), skipMany1, (<|>), getInput, lookAhead)
import           Text.Parsec.Char (string, char, satisfy, spaces, digit, alphaNum, letter, space)
import           Text.Parsec.Combinator (notFollowedBy)
import           Text.Parsec.Error
import           Text.Parsec.Prim (unexpected)
import           Text.Parsec.String (Parser)

import           Db.Db (Row(..), UserId, Username, Email)

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
metaCommandParser = choice $ fmap (\(fst, snd) -> try $ snd <$ (spaces *> string fst)) metaCommands

-- metaCommandParser = choice $ fmap (try . uncurry ($>) . first string)  metaCommands

metaParser :: Parser Char
metaParser = char '.'

data Statement = Select | Insert Row deriving (Show, Eq)

data StatementParserError = UnrecognizedStatement String
                         | SyntaxError
                         | StringTooLong

instance Show StatementParserError where
  show (UnrecognizedStatement i) = convertString
                                   $ Fmt.format "Unrecognized keyword at start of '{}'."
                                   (Fmt.Only i)
  show SyntaxError = "Syntax error. Could not parse statement."
  show StringTooLong = "String is too long."

-- statementTypeParser :: Parser String
-- statementTypeParser = choice
--                       [ string "select"
--                       , string "insert"]

selectParser :: Parser Statement
selectParser = do
  i <- getInput
  _ <- string "select" <|> fail (show $ UnrecognizedStatement i)
  pure Select

insertParser :: Parser Statement
insertParser = do
    i <- getInput
    _ <- try (string "insert") <|> fail (show $ UnrecognizedCommand i)
    (userId, name, email) <- parseParams
    pure $ Insert $ Row userId name email
  where
    parseParams :: Parser (UserId, Username, Email)
    parseParams = do
      _      <- skipMany1 space            <|> fail (show SyntaxError)
      userId <- many1 digit                <|> fail (show SyntaxError)
      _      <- skipMany1 space            <|> fail (show SyntaxError)
      name   <- parseName
      email  <- many1 alphaNum             <|> fail (show SyntaxError)
      pure (read userId, name, email)

    parseName :: Parser Username
    parseName = do
      name   <- count' 1 32 letter                  <|> fail (show SyntaxError)
      _      <- notFollowedBy' (lookAhead alphaNum) <|> fail (show StringTooLong)
      _      <- skipMany1 space                     <|> fail (show SyntaxError)
      pure name

statementParser :: Parser Statement
statementParser = choice [ selectParser, insertParser ]

parse' :: Parser a -> String -> Either ParseError a
parse' p = parse p ""

-- See https://github.com/haskell/parsec/issues/8
notFollowedBy' :: (Stream s m t, Show a) => ParsecT s u m a -> ParsecT s u m ()
notFollowedBy' p = try $ join $
      do {a <- try p; return (unexpected (show a));}
  <|> return (return ())
