{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

import Lib1 qualified
import Lib2 qualified
import Lib2 (Query(CreateOrder), Order (OrderObject))
import Lib3 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [ testCase "Simple order query" $
      Lib2.parseQuery "Order Pizza for table 12"
      @?= Right (CreateOrder (OrderObject {
        Lib2.command = Lib2.Order,
        Lib2.dishList = [Lib2.MainCourse Lib2.Pizza],
        Lib2.tableNumber = Just 12,
        Lib2.paymentInfo = Nothing,
        Lib2.tipAmount = Nothing,
        Lib2.editOrder = Nothing})),
    testCase "Empty parse string" $
      Lib2.parseQuery "" @?= Left "Empty string",
    testCase "Complex order test" $
      Lib2.parseQuery "Order Pizza for table 12 pay with cash tip 12$ edit order: [ Order Pasta ]"
      @?= Right (CreateOrder (OrderObject {
        Lib2.command = Lib2.Order,
        Lib2.dishList = [Lib2.MainCourse Lib2.Pizza],
        Lib2.tableNumber = Just 12,
        Lib2.paymentInfo = Just Lib2.PayWithCash,
        Lib2.tipAmount = Just (Lib2.TipAmount "12$"),
        Lib2.editOrder = Just (OrderObject {
          Lib2.command = Lib2.Order,
          Lib2.dishList = [Lib2.MainCourse Lib2.Pasta],
          Lib2.tableNumber = Nothing,
          Lib2.paymentInfo = Nothing,
          Lib2.tipAmount = Nothing,
          Lib2.editOrder = Nothing
          })
        })),
    testCase "Not existing command" $
      Lib2.parseQuery "Edit Pizza for table 12"
      @?= Left "Command 'Edit' does not exist",
    testCase "Not existing dish" $
      Lib2.parseQuery "Order Zepelinai for table 12"
      @?= Left "'Zepelinai' dish does not exist"
  ]
propertyTestStrings :: [String]
propertyTestStrings = [
  "BEGIN Order Pizza for table 12; Order Pizza for table 13 END", 
  "BEGIN Order Pizza for table 54 END",
  "BEGIN Order Pizza, Cola for table 99 pay with cash tip 12$ edit order: [ Remove Pizza ] END",
  "BEGIN Order Garlic Bread for table 12 END",
  "BEGIN Order Garlic Bread, Fries, Soup for table 84 pay with cash edit order: [ Add Cola ]; Order Steak, Sushi for table 45 edit order: [ Remove Sushi ] END",
  "BEGIN Order Garlic Bread for table 1; Order Fries for table 2; Order Soup for table 3; Order Pizza for table 4 END",
  "BEGIN Add Pizza for table 12 END",
  "BEGIN Remove Pizza, Pasta, Pasta, Water for table 12 END",
  "BEGIN Order Pizza for table 12 edit order: [ Add Pizza for table 12 ] END"
-- "BEGIN Add Pizza for table 12; Remove Pizza for table 12; Order Pizza for table 12 edit order: [ Add Pizza for table 12 ] END"
  ]

propertyTests :: TestTree
propertyTests = 
  testGroup "Render and parse statement tests" (map (
    \testString -> QC.testProperty "Test" $ 
      fmap (Lib3.renderStatements . fst) (Lib3.parseStatements testString) == Right testString) propertyTestStrings)
  
