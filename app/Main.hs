{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.String.Conversions
import Data.Text
import qualified Data.Text.Format as Fmt
import Lib
import Text.Parsec (parse)
import Text.Parsec.Char (string)
import Text.Parsec.Error
import Text.Parsec.Text (Parser)
import System.Exit
import System.IO


cmdParser :: Text -> Parser Text
cmdParser t = convertString <$> string (convertString t)

exitParser :: Parser Text
exitParser = cmdParser ".exit"

parse' :: Parser a -> Text -> Either ParseError a
parse' p i = parse p "" i

main :: IO ()
main =
  forever $ do
    printPrompt
    input <- convertString <$> getLine :: IO Text
    case parse exitParser "" input of
      Left e -> do
        Fmt.print "Unrecognized command '{}'\n" (Fmt.Only input)
        main
      Right p -> exitSuccess

printPrompt :: IO ()
printPrompt = do
  putStr "db > "
  hFlush stdout
