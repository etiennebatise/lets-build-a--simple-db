{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad
import Data.Either (either)
import Data.List (lookup)
import Data.String.Conversions
import Data.Text
import qualified Data.Text.Format as Fmt
import Lib
import Text.Parsec (Parsec, parse, try, choice, token)
import Text.Parsec.Char (string, char, satisfy)
import Text.Parsec.Error
import Text.Parsec.String (Parser)
import System.Exit
import System.IO

data MetaCommand = Exit
  deriving (Read)

instance Show MetaCommand where
  show Exit = ".exit"


metaCommands :: [(String, MetaCommand)]
metaCommands =
  [ (".exit", Exit) ]

metaCommandParser :: Parser MetaCommand
metaCommandParser = choice $ fmap (\(fst, snd) -> try $ snd <$ string fst) metaCommands

metaParser :: Parser Char
metaParser = char '.'

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

    handleStatement i = print "TODO"


executeMetaCommand :: MetaCommand -> IO ()
executeMetaCommand = \case
  Exit -> exitSuccess


printPrompt :: IO ()
printPrompt = do
  putStr "db > "
  hFlush stdout
