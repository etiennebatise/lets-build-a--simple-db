import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parserTests, dbTests, executionTests]

parserTests = testGroup "Parser" []
dbTests = testGroup "DB" []
executionTests = testGroup "Execution" []

