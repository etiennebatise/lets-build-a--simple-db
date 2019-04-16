{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
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
import Data.List (lookup)
import Data.String.Conversions
import qualified Data.Text.Format as Fmt
import Data.Tuple (uncurry, swap)
import Data.Word
import Lib
import Text.Parsec (Parsec, parse, try, choice, token, many, many1)
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

type UserId = Int32
type Username = String
type Email = String

type SizeInBytes = Int64

idSizeInBytes ::SizeInBytes
idSizeInBytes = 8

usernameSizeInBytes ::SizeInBytes
usernameSizeInBytes = 32

emailSizeInBytes ::SizeInBytes
emailSizeInBytes = 255

resize :: B.ByteString -> SizeInBytes-> B.ByteString
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
  serialize row = let i = Bld.putInt32be $ rid row
                      u = Bld.fromByteString $ resize (C.pack $ name row) usernameSizeInBytes
                      e = Bld.fromByteString $ resize (C.pack $ email row) emailSizeInBytes
                      total = Bld.append (Bld.append i u) e
                  in Bld.toLazyByteString total
  deserialize = runGet $ do
      index <- Get.getInt32be
      rem <- Get.getRemainingLazyByteString
      let name = C.unpack $ LB.toStrict $ LB.dropWhile (== 0) $ LB.take usernameSizeInBytes rem
          email = C.unpack $ LB.toStrict $ LB.dropWhile (== 0) $ LB.drop 32 rem
      pure $ Row index name email

-- columnUsernameSize :: Int
-- columnUsernameSize = 32

-- columnEmailSize :: Int
-- columnEmailSize = 255

-- idOffset = 0;
-- usernameOffset = idOffset + idSize;
-- emailOffset = usernameOffset + usernameSize;
-- rowSize = idSize + usernameSize + emailSize;


---------------
-- EXECUTION --
---------------
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
  Insert (Row uid name email) -> print "This is where we would do an insert.\n"
  Select -> print "This is where we would do an select.\n"

printPrompt :: IO ()
printPrompt = do
  putStr "db > "
  hFlush stdout
