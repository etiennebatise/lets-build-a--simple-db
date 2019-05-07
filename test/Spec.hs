import Test.Tasty

import Data.Either (isLeft)
import Db.Db (Row(..), usernameSize)
import Db.Parser (parse', insertParser, Statement(..))
import Test.Hspec.Expectations
import Test.Tasty.HUnit
import Text.Parsec (parse)
import Text.Parsec.Error
import Text.Parsec.String (Parser)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parserTests, dbTests, executionTests]

parserTests = testGroup "Parser" [insertParserTest]

insertParserTest = testGroup "Insert" insertParserTestCases

insertParserTestCases =
  [ testCase "should parse a valid statement" $
    parse' insertParser "insert 1 foo bar" `shouldBe` (Right $ Insert $ Row 1 "foo" "bar")

  , testCase "should parse string that are the maximum length" $
    let longName = replicate usernameSize' 'a'
        statement = "insert 1 " ++ longName ++ " bar"
        result = parse' insertParser statement
    in
        result `shouldBe` (Right $ Insert $ Row 1 longName "bar")

  , testCase "should fail when strings are too long" $
    let tooLong = replicate (usernameSize' + 1) 'a'
        statement = "insert 1 " ++ tooLong ++ " bar"
        Left parseError = parse' insertParser statement
        errorMessage = messageString $ last $ errorMessages parseError
    in
        errorMessage `shouldBe` "String is too long."
  ]

dbTests = testGroup "DB" []
executionTests = testGroup "Execution" []


usernameSize' = fromIntegral usernameSize