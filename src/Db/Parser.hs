{-# LANGUAGE OverloadedStrings #-}

module Db.Parser where

import          Control.Monad.Combinators (count')
import          Control.Monad (join)
import           Data.String.Conversions
import qualified Data.Text.Format as Fmt
import           Text.Parsec (Parsec, ParsecT, Stream, parse, try, choice, token, many, many1, (<?>), skipMany, skipMany1, (<|>), getInput, lookAhead)
import           Text.Parsec.Char (string, char, satisfy, spaces, digit, alphaNum, letter, space)
import           Text.Parsec.Combinator (notFollowedBy)
import           Text.Parsec.Error
import           Text.Parsec.Prim (unexpected)
import           Text.Parsec.String (Parser)

import           Db.Db (Row(..), UserId, Username, Email, usernameSize, emailSize)

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
                         | IdNegative

instance Show StatementParserError where
  show (UnrecognizedStatement i) = convertString
                                   $ Fmt.format "Unrecognized keyword at start of '{}'."
                                   (Fmt.Only i)
  show SyntaxError = "Syntax error. Could not parse statement."
  show StringTooLong = "String is too long."
  show IdNegative = "ID must be positive."


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
      userId <- parseId
      _      <- skipMany1 space            <|> fail (show SyntaxError)
      name   <- parseName
      email  <- parseEmail

      pure (userId, name, email)

    parseId :: Parser UserId
    parseId = do
      _      <- notFollowedBy' (lookAhead $ char '-') <|> fail (show IdNegative)
      userId <- many1 digit
      pure $ read userId

    parseName :: Parser Username
    parseName = do
      name <- count' 1 usernameSize' letter       <|> fail (show SyntaxError)
      _    <- notFollowedBy' (lookAhead alphaNum) <|> fail (show StringTooLong)
      _    <- skipMany1 space                     <|> fail (show SyntaxError)
      pure name

    parseEmail :: Parser Email
    parseEmail = do
      email <- count' 1 emailSize' letter          <|> fail (show SyntaxError)
      _     <- notFollowedBy' (lookAhead alphaNum) <|> fail (show StringTooLong)
      _     <- spaces                              <|> fail (show SyntaxError)
      _     <- notFollowedBy' (lookAhead alphaNum) <|> fail (show SyntaxError)
      pure email

    usernameSize' = fromIntegral usernameSize

    emailSize' = fromIntegral emailSize

statementParser :: Parser Statement
statementParser = choice [ selectParser, insertParser ]

parse' :: Parser a -> String -> Either ParseError a
parse' p = parse p ""

-- See https://github.com/haskell/parsec/issues/8
notFollowedBy' :: (Stream s m t, Show a) => ParsecT s u m a -> ParsecT s u m ()
notFollowedBy' p = try $ join $
      do {a <- try p; return (unexpected (show a));}
  <|> return (return ())
