{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
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

sampleOrder :: Lib2.Order
sampleOrder = OrderObject {
  Lib2.command = Lib2.Order,
  Lib2.dishList = [Lib2.MainCourse Lib2.Pizza],
  Lib2.tableNumber = Just 12,
  Lib2.paymentInfo = Just Lib2.PayWithCash,
  Lib2.tipAmount = Just (Lib2.TipAmount "12$"),
  Lib2.editOrder = Nothing }

addSampleDish :: Order -> Order
addSampleDish order@OrderObject{Lib2.dishList=d,Lib2.editOrder=e} = 
  let 
    newDishList = if null d then sampleDishList else d
    newEditOrder = case e of
      Nothing -> Nothing
      Just order2 -> Just (addSampleDish order2)
  in
    order{Lib2.dishList=newDishList,Lib2.editOrder=newEditOrder}

genTableNumber :: Order -> Order
genTableNumber order@OrderObject{Lib2.tableNumber=t,Lib2.editOrder=e} = 
  let 
    newTableNumber = case t of
      Nothing -> Nothing
      Just num -> if num < 0 then Just (-num) else Just num
    newEditOrder = case e of
      Nothing -> Nothing
      Just order2 -> Just (genTableNumber order2)
  in
    order{Lib2.tableNumber=newTableNumber,Lib2.editOrder=newEditOrder}

sampleDishList :: [Lib2.Dish]
sampleDishList = [Lib2.MainCourse Lib2.Pasta]

genTestString :: [Lib2.Order] -> String -> String
genTestString [] _ = genTestString [sampleOrder] "BEGIN "
genTestString (h:t) testString =
  let 
    order = genTableNumber (addSampleDish h)
  in 
    if null t then testString ++ show order ++ " END" else genTestString t (testString ++ show order ++ "; ")

testOrder :: [Lib2.Order] -> Bool
testOrder orders =  
  let
    testString = genTestString orders "BEGIN "
  in
    fmap (Lib3.renderStatements . fst) (Lib3.parseStatements testString) == Right testString

instance Arbitrary Lib2.Command where
  arbitrary = elements [Lib2.Order, Lib2.Add, Lib2.Remove]

instance Arbitrary Lib2.MainCourse where
  arbitrary = elements [Lib2.Pizza, Lib2.Burger, Lib2.Pasta, Lib2.Salad, Lib2.Steak, Lib2.Sushi]

instance Arbitrary Lib2.SideDish where
  arbitrary = elements [Lib2.Fries, Lib2.GarlicBread, Lib2.Soup]

instance Arbitrary Lib2.Beverage where
  arbitrary = elements [Lib2.Cola, Lib2.Water, Lib2.Juice, Lib2.Wine, Lib2.Beer]

instance Arbitrary Lib2.PaymentInfo where
  arbitrary = elements [Lib2.PayWithCard, Lib2.PayWithCash]

instance Arbitrary Lib2.Dish where
  arbitrary = oneof [Lib2.MainCourse <$> arbitrary, Lib2.SideDish <$> arbitrary, Lib2.Beverage <$> arbitrary]

instance Arbitrary Lib2.TipAmount where
  arbitrary = return (Lib2.TipAmount "12$")

instance Arbitrary Lib2.Order where
  arbitrary = Lib2.OrderObject <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary 

propertyTests :: TestTree
propertyTests = 
  testGroup "Render and parse statement tests" [
    do
      QC.testProperty "Render and parse statement PROPERTY tests" testOrder
  ]
