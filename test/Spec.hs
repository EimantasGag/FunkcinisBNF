{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified
import Lib2 qualified
import Lib2 (Query(CreateOrder), Order (OrderObject))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

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