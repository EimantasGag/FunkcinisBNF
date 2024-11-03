{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition,
    Order(..),
    Command(..),
    MainCourse(..),
    SideDish(..),
    Beverage(..),
    Dish(..),
    TipAmount(..),
    PaymentInfo(..),
    TableNumber,
    ) where

import qualified Data.Char as C
import qualified Data.List as L

-- <order> ::= <command> " " <dish_list> (" " <table_number>)? (" " <payment_info>)? (" " <tip>)? (" edit order: [ " <order> " ]")? +

-- <command> ::= "Order" | "Add" | "Remove" +

-- <dish_list> ::= <dish> | <dish> ", " <dish_list> + 

-- <dish> ::= <main_course> | <side_dish> | <beverage> +

-- <main_course> ::= "Pizza" | "Burger" | "Pasta" | "Salad" | "Steak" | "Sushi" +

-- <side_dish> ::= "Fries" | "Garlic Bread" | "Salad" | "Soup" + 

-- <beverage> ::= "Cola" | "Water" | "Juice" | "Wine" | "Beer" +

-- <table_number> ::= "for table " <number> +

-- <tip> ::= "tip " <amount> +

-- <amount> ::= [0-9]+ ("." [0-9] [0-9])* "$" +

-- <number> ::= <digit> | <digit> <digit> + 

-- <digit> ::= [0-9] +

-- <payment_info> ::= "pay with card" | "pay with cash" +

type Parser a = String -> Either String (a, String)

type ParserQuery a = String -> Either String a 

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query = CreateOrder Order | ViewOrder TableNumber | ViewOrders | CancelOrder TableNumber
  deriving (Show, Eq)
-- | The instances are needed basically for tests
-- instance Eq Query where
--   (==) _ _= False

-- instance Show Query where
--   show _ = ""

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State = State {
  ordersList :: [Order]
}
  deriving (Show, Eq)

-- <order> ::= <command> " " <dish_list> (" " <table_number>)? (" " <payment_info>)? (" " <tip>)? (" edit order: [ " <order> " ]")?
data Order = OrderObject {
  command :: Command,
  dishList :: [Dish],
  tableNumber :: Maybe Int,
  paymentInfo :: Maybe PaymentInfo,
  tipAmount :: Maybe TipAmount,
  editOrder :: Maybe Order
} deriving (Show, Eq)

data Command = Order | Add | Remove
  deriving (Show, Eq)

data MainCourse = Pizza | Burger | Pasta | Salad | Steak | Sushi
  deriving (Show, Eq)

data SideDish = Fries | GarlicBread | Soup
  deriving (Show, Eq)

data Beverage = Cola | Water | Juice | Wine | Beer
  deriving (Show, Eq)

data Dish = MainCourse MainCourse | SideDish SideDish | Beverage Beverage
  deriving (Eq) 

instance Show Dish where
  show (MainCourse m) = show m
  show (SideDish s) = show s
  show (Beverage b) = show b

data TipAmount = TipAmount String
  deriving (Eq)

instance Show TipAmount where
  show (TipAmount amount) = amount

data PaymentInfo = PayWithCard | PayWithCash
  deriving (Eq)

instance Show PaymentInfo where
  show PayWithCard = "paying with card"
  show PayWithCash = "paying with cash"

type TableNumber = Int

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = State {ordersList=[]}

-- | Parses user's input.
-- The function must have tests.
-- <parseQuery> ::= <order> | "view orders" | "view order table " <number> | "cancel order " <table_number>
parseQuery :: ParserQuery Query
parseQuery input = case parseOrder "" input of
  Right(order, rest) -> if rest == "" then Right (CreateOrder order) else Left ("Unexpected characters after order: " ++ rest)
  Left parseOrderError -> 
    case (or3Query parseViewOrders parseViewOrder parseCancelOrder) input of
      Right q -> Right q
      Left _ -> Left parseOrderError

parseCancelOrder :: ParserQuery Query
parseCancelOrder input = case (and3 (\a b c -> c) (parseCertainNWords 2 "" "cancel order") parseWhitespace parseTableNumber) input of
  Right(tableNumber, rest) -> if rest == "" then Right (CancelOrder tableNumber) else Left ("Unexpected characters after view orders: " ++ rest) 
  Left err -> Left err

parseViewOrders :: ParserQuery Query
parseViewOrders input = case (parseCertainNWords 2 "" "view orders" input) of
  Right (v, rest) -> if rest == "" then Right ViewOrders else Left ("Unexpected characters after view orders: " ++ rest)
  Left err -> Left err

parseViewOrder :: ParserQuery Query
parseViewOrder input = case (and3 (\_ _ c -> c) (parseCertainNWords 3 "" "view order table") parseWhitespace parseNumber) input of
  Right (tableNumber, rest) -> if rest == "" then Right (ViewOrder tableNumber) else Left ("Unexpected characters after view orders: " ++ rest)
  Left err -> Left err

findOrderByTable :: [Order] -> TableNumber -> Maybe Order
findOrderByTable [] wantedTableNumber = Nothing 
findOrderByTable (curOrder@OrderObject{command=command,tableNumber=tableNumber}:t) wantedTableNumber = 
  case tableNumber of
    Just number -> if wantedTableNumber == number && command == Order then Just curOrder else findOrderByTable t wantedTableNumber
    Nothing -> findOrderByTable t wantedTableNumber

printPaymentInfo :: Maybe PaymentInfo -> String -> String
printPaymentInfo p resString = case p of
  Just paymentInfo -> resString ++ "PAYMENT INFORMATION: " ++ show paymentInfo ++ "\n"
  Nothing -> resString ++ "PAYMENT INFORMATION: not provided \n"

printTipAmount :: Maybe TipAmount -> String -> String
printTipAmount t resString = case t of
  Just tipAmount -> resString ++ "TIP AMOUNT: " ++ show tipAmount ++ "\n"
  Nothing -> resString ++ "TIP AMOUNT: 0$ \n"

getAllMainCourse :: [Dish] -> [Dish] -> [Dish]
getAllMainCourse [] res = res
getAllMainCourse (h:t) res =
  case h of
    MainCourse dish -> getAllMainCourse t (MainCourse dish : res)
    _ -> getAllMainCourse t res

getAllSideDish :: [Dish] -> [Dish] -> [Dish]
getAllSideDish [] res = res
getAllSideDish (h:t) res =
  case h of
    SideDish dish -> getAllSideDish t (SideDish dish : res)
    _ -> getAllSideDish t res

getAllBeverage :: [Dish] -> [Dish] -> [Dish]
getAllBeverage [] res = res
getAllBeverage (h:t) res =
  case h of
    Beverage dish -> getAllBeverage t (Beverage dish : res)
    _ -> getAllBeverage t res

printDishList :: [Dish] -> String -> String
printDishList [] res = res
printDishList dishList@(h:t) res = printDishList t (res ++ show h ++ if t /= [] then ", " else "")

printDishes :: [Dish] -> String
printDishes dishList = 
  "DISHES: " ++ "\n" ++ 
  "    MAIN COURSE: " ++ printDishList (getAllMainCourse dishList []) "" ++ "\n" ++ 
  "    SIDE DISH: " ++ printDishList (getAllSideDish dishList []) "" ++ "\n" ++ 
  "    BEVERAGE: " ++ printDishList (getAllBeverage dishList []) "" ++ "\n"

getTableNumber :: Order -> Maybe TableNumber
getTableNumber OrderObject{tableNumber=tableNumber} = tableNumber 

inNumberList :: Int -> [Int] -> Bool
inNumberList number [] = False
inNumberList number (h:t) = if number == h then True else inNumberList number t 

printAllOrders :: [Order] -> [TableNumber] -> String -> String
printAllOrders [] seenTableNumbers res = res
printAllOrders ordersList@(h:t) seenTableNumbers res =
  case getTableNumber h of
    Just tableNumber -> 
      if inNumberList tableNumber seenTableNumbers 
        then printAllOrders t seenTableNumbers res 
        else printAllOrders t (tableNumber :seenTableNumbers) (res ++ "Printing current order for table #" ++ show tableNumber ++ ":\n" ++ printOrder h)
    Nothing -> printAllOrders t seenTableNumbers res

printOrder :: Order -> String
printOrder OrderObject{command=c,dishList=d,tableNumber=t,paymentInfo=p,tipAmount=tip} = 
  let
    resString = printDishes d
  in
    printTipAmount tip (printPaymentInfo p resString)

addDishes :: [Order] -> Order -> TableNumber -> Maybe [Order]
addDishes orderList OrderObject{dishList=dishList} tableNumber = 
  let
    orderToAddTo = findOrderByTable orderList tableNumber
  in
    case orderToAddTo of
      Just order@OrderObject{dishList=dishList2} -> Just ([order{dishList=dishList ++ dishList2}] ++ orderList)
      Nothing -> Nothing

inDishList :: Dish -> [Dish] -> Bool
inDishList dish [] = False
inDishList dish (h:t) = if dish == h then True else inDishList dish t 

removeDishesHelper :: [Dish] -> [Dish] -> [Dish] -> [Dish]
removeDishesHelper [] removeDishList res = res
removeDishesHelper (h:t) removeDishList res =
  case inDishList h removeDishList of
    True -> removeDishesHelper t removeDishList res
    False -> removeDishesHelper t removeDishList (h : res)

removeDishes :: [Order] -> Order -> TableNumber -> Maybe [Order]
removeDishes orderList OrderObject{dishList=removeDishList} tableNumber =
  let
    orderRemoveFrom = findOrderByTable orderList tableNumber
  in
    case orderRemoveFrom of
      Just order@OrderObject{dishList=dishList} -> Just ([order{dishList=removeDishesHelper dishList removeDishList []}] ++ orderList)
      Nothing -> Nothing

doEditOrder :: Order -> Either String Order
doEditOrder order@OrderObject{command=c,dishList=d,tableNumber=t,paymentInfo=p,tipAmount=tip,editOrder=e} = 
  case e of
    Just editOrder@OrderObject{command=c2,dishList=d2,tableNumber=t2,paymentInfo=p2,tipAmount=tip2,editOrder=e2} -> 
      case c2 of
        Order -> 
          case c of
            Add -> Left "Cannot do edit order: [...], when parent order is of type Add"
            Remove -> Left "Cannot do edit order: [...], when parent order is of type Remove"
            Order -> 
              let 
                editedOrder = OrderObject{
                                command=c2,
                                dishList=d2,
                                tableNumber=if t2 /= Nothing then t2 else t,
                                paymentInfo=if p2 /= Nothing then p2 else p,
                                tipAmount=if tip2 /= Nothing then tip2 else tip,
                                editOrder=e2
                              }
              in
                if e2 == Nothing then Right editedOrder else doEditOrder editedOrder
        Add -> 
          let 
            editedOrder = OrderObject{
                              command=c,
                              dishList=d2 ++ d,
                              tableNumber=t,
                              paymentInfo=p,
                              tipAmount=tip,
                              editOrder=e2
                            }
          in
            if e2 == Nothing then Right editedOrder else doEditOrder editedOrder
        Remove -> 
          let 
            editedOrder = OrderObject{
                              command=c,
                              dishList=removeDishesHelper d d2 [],
                              tableNumber=t,
                              paymentInfo=p,
                              tipAmount=tip,
                              editOrder=e2
                            }
          in
            if e2 == Nothing then Right editedOrder else doEditOrder editedOrder
    Nothing -> Right order 

removeOrder :: [Order] -> TableNumber -> [Order] -> [Order]
removeOrder [] tableNumber res = res
removeOrder (h:t) tableNumber res =
  if getTableNumber h == Just tableNumber then removeOrder t tableNumber res else removeOrder t tableNumber (h : res)  


-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state@State{ordersList=ordersList} query = case query of
  CreateOrder order@OrderObject{command=command,tableNumber=tableNumber} -> 
    case doEditOrder order of
      Right order -> case command of
        Order -> case tableNumber of 
          Just _ -> Right (Just "New order placed succesfully", State{ordersList = [order] ++ ordersList})
          Nothing -> Left "Failed to place a new order. New order has to provide a table number it is placed for"
        Add -> case tableNumber of
          Just number -> case addDishes ordersList order number of 
            Just updatedOrdersList -> Right (Just "Dishes added succesfully", State{ordersList = updatedOrdersList})
            Nothing -> Left ("No order placed for table #" ++ show number)
          Nothing -> Left "Failed to add dishes. To add dishes to an order, a table number has to be provided"
        Remove -> case tableNumber of
          Just number -> case removeDishes ordersList order number of
            Just updatedOrdersList -> Right (Just "Dishes removed succesfully", State{ordersList = updatedOrdersList})
            Nothing -> Left ("No order placed for table #" ++ show number)
          Nothing -> Left "Failed to remove dishes. To remove dishes from an order, a table number has to be provided"
      Left falseEditOrderMsg -> Left falseEditOrderMsg
  ViewOrders -> Right (Just (printAllOrders ordersList [] ""), state)
  ViewOrder tableNumber -> case findOrderByTable ordersList tableNumber of
    Just order -> Right (Just ("Printing current order for table #" ++ show tableNumber ++ ":\n" ++ printOrder order), state) 
    Nothing -> Right (Just ("Order is not placed for table #" ++ show tableNumber), state)
  CancelOrder tableNumber -> case findOrderByTable ordersList tableNumber of
    Just _ -> Right(Just ("Order for table #" ++ show tableNumber ++ " cancel succesfully"), State{ordersList = removeOrder ordersList tableNumber []})
    Nothing -> Right (Just ("No order placed for table #" ++ show tableNumber), state)

parseWord :: Parser String
parseWord input =
    let letters = L.takeWhile C.isLetter input
        rest = L.drop (length letters) input
    in if not (null letters)
        then Right (letters, rest)
        else if input /= "" then Left (input ++ " does not start with a letter") else Left ("Empty string")

-- <main_course> ::= "Pizza" | "Burger" | "Pasta" | "Salad" | "Steak" | "Sushi"
parseMainCourse :: Parser Dish
parseMainCourse input = 
  case parseWord input of
    Right (word, rest) -> case word of
      "Pizza" -> Right (MainCourse Pizza, rest)
      "Burger" -> Right (MainCourse Burger, rest)
      "Pasta" -> Right (MainCourse Pasta, rest)
      "Salad" -> Right (MainCourse Salad, rest)
      "Steak" -> Right (MainCourse Steak, rest)
      "Sushi" -> Right (MainCourse Sushi, rest)
      _ -> Left ("'" ++ word ++ "' dish does not exist")
    Left e -> Left e

-- <side_dish> ::= "Fries" | "Garlic Bread" | "Soup"
parseSideDish :: Parser Dish
parseSideDish input = 
  case parseWord input of
    Right (word, rest) -> case word of
      "Fries" -> Right (SideDish Fries, rest)
      "Soup" -> Right (SideDish Soup, rest)
      _ -> 
        case parseNWords 2 "" input of
        Right (word, rest) -> case word of
          "Garlic Bread" -> Right(SideDish GarlicBread, rest) 
          _ -> Left ("'" ++ word ++ "' dish does not exist")
        _ -> Left "<parseSideDishError> Error reading side dish"
    Left _ -> Left "<parseSideDishError> Error reading side dish"

-- <beverage> ::= "Cola" | "Water" | "Juice" | "Wine" | "Beer" 
parseBeverage :: Parser Dish
parseBeverage input = 
  case parseWord input of
    Right (word, rest) -> case word of
      "Cola" -> Right (Beverage Cola, rest)
      "Water" -> Right (Beverage Water, rest)
      "Juice" -> Right (Beverage Juice, rest)
      "Wine" -> Right (Beverage Wine, rest)
      "Beer" -> Right (Beverage Beer, rest)
      _ -> Left ("'" ++ word ++ "' dish does not exist")
    Left e -> Left e

-- <command> ::= "Order" | "Add" | "Remove"
parseCommand :: Parser Command
parseCommand input = 
  case parseWord input of
    Right (word, rest) -> case word of
      "Order" -> Right (Order, rest)
      "Add" -> Right (Add, rest)
      "Remove" -> Right (Remove, rest)
      _ -> Left ("Command '" ++ word ++ "' does not exist")
    Left e -> Left e

-- parses whitespace
parseWhitespace :: Parser String
parseWhitespace [] = Right ("", [])
parseWhitespace (h : t) = if C.isSpace h then Right (" ", t) else Left "missing whitespace"

-- parse n number of words separated by whitespace
parseNWords :: Int -> String -> Parser String
parseNWords 0 res input = Right (res, input)
parseNWords n res input =
    case parseWhitespace input of
      Left _ -> 
        case parseWord input of
            Left e -> Left e
            Right (word, rest) -> parseNWords (n-1) (res ++ word) rest
      Right (space, rest) ->
        case parseWord (drop 1 input) of
            Left e -> Left e
            Right (word, rest) -> parseNWords (n-1) (res ++ " " ++ word) rest

-- parse n number of words separated by whitespace
parseCertainNWords :: Int -> String -> String -> Parser String
parseCertainNWords 0 res match input = if match == res then Right (res, input) else Left res
parseCertainNWords n res match input =
    case parseWhitespace input of
      Left _ -> 
        case parseWord input of
            Left e -> Left e
            Right (word, rest) -> parseCertainNWords (n-1) (res ++ word) match rest
      Right (space, rest) ->
        case parseWord (drop 1 input) of
            Left e -> Left e
            Right (word, rest) -> parseCertainNWords (n-1) (res ++ " " ++ word) match rest

-- <payment_info> ::= "pay with card" | "pay with cash"
parsePaymentInfo :: Parser PaymentInfo
parsePaymentInfo input = 
  case parseNWords 3 "" input of
    Right (word, rest) -> case word of
      "pay with card" -> Right (PayWithCard, rest)
      "pay with cash" -> Right (PayWithCash, rest)
      _ -> Left "incorrect payment info"
    Left e -> Left e

-- parsing char
parseChar :: Parser Char
parseChar [] = Left "Empty string"
parseChar (h:t) = Right (h, t)

-- parsing certain char
parseCertainChar :: Char -> Parser Char
parseCertainChar c [] = Left "Empty string"
parseCertainChar c (h:t) = if h == c then Right (h, t) else Left ("Unexpected char: " ++ [c])

-- <amount> ::= [0-9]+ ("." [0-9] [0-9])* "$"
parseAmount :: Parser TipAmount
parseAmount [] = Left "empty input, cannot parse amount"
parseAmount str =
    let
        digits = L.takeWhile C.isDigit str
        rest = drop (length digits) str
    in
        case digits of
            [] -> Left "not a number"
            _ -> case parseChar rest of
                Left e -> Left "amount has to end with $ sign"
                Right (char, rest) -> case char of
                    '.' -> case parse2Digits rest of
                        Left e -> Left "fractional number has to be 2 digit"
                        Right (digit2, rest) -> case parseChar rest of
                            Left e -> Left "amount has to end with $ sign"
                            Right (char, rest) -> case char of
                                '$' -> Right(TipAmount (digits ++ "." ++ digit2 ++ "$"), rest)
                                _ -> Left "amount has to end with $ sign"
                    '$' -> Right(TipAmount (digits ++ "$"), rest)
                    _ -> Left ("unexpected symbol: " ++ [char])

-- parse only 2 digit numbers 
-- (helper function for parseAmount; returns String because 01 is not the same as 1)
parse2Digits :: Parser String
parse2Digits str =
    let
        digits = L.takeWhile C.isDigit str
        rest = drop (length digits) str
    in
        if length digits /= 2 then Left "number is not 2 digit"
        else 
            case digits of
                [] -> Left "not a number"
                _ -> Right (digits, rest)

-- <number> ::= <digit> | <digit> <digit>
parseNumber :: Parser Int
parseNumber [] = Left "<parseNumberError> empty input, cannot parse a number"
parseNumber str = ((and2 (\a b -> a*10+b) parseDigit parseDigit) `or2` parseDigit) str 

or2 :: Parser a -> Parser a -> Parser a
or2 a b = \input ->
    case a input of
        Right r1 -> Right r1
        Left e1 ->
            case b input of
                Right r2 -> Right r2
                Left e2 -> Left e1

or3Query :: ParserQuery a -> ParserQuery a -> ParserQuery a -> ParserQuery a
or3Query a b c = \input ->
    case a input of
        Right r1 -> Right r1
        Left e1 ->
            case b input of
                Right r2 -> Right r2
                Left e2 -> 
                  case c input of
                    Right r3 -> Right r3
                    Left e3 -> Left e3
                


and2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2 f a b = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) -> Right (f v1 v2, r2)
                Left e2 -> Left e2
        Left e1 -> Left e1

and3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3 f a b c = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) ->
                    case c r2 of
                        Right (v3, r3) -> Right (f v1 v2 v3, r3)
                        Left e3 -> Left e3
                Left e2 -> Left e2
        Left e1 -> Left e1

and4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
and4 f a b c d = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) ->
                    case c r2 of
                        Right (v3, r3) -> 
                          case d r3 of
                            Right (v4, r4) -> Right (f v1 v2 v3 v4, r4)
                            Left e4 -> Left e4
                        Left e3 -> Left e3
                Left e2 -> Left e2
        Left e1 -> Left e1

and5 :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
and5 f a b c d e = \input ->
    case a input of
        Right (v1, r1) ->
            case b r1 of
                Right (v2, r2) ->
                    case c r2 of
                        Right (v3, r3) -> 
                          case d r3 of
                            Right (v4, r4) -> 
                              case e r4 of
                                Right (v5, r5) -> Right (f v1 v2 v3 v4 v5, r5)
                                Left e5 -> Left e5
                            Left e4 -> Left e4
                        Left e3 -> Left e3
                Left e2 -> Left e2
        Left e1 -> Left e1

-- <table_number> ::= "for table " <number>
parseTableNumber :: Parser TableNumber
parseTableNumber input = (and3 (\_ _ c -> c) (parseCertainNWords 2 "" "for table") parseWhitespace parseNumber) input

-- <tip> ::= "tip " <amount>
parseTip :: Parser TipAmount
parseTip input = (and3 (\_ _ b -> b) (parseCertainNWords 1 "" "tip") parseWhitespace parseAmount) input

-- <dish_list> ::= <dish> | <dish> ", " <dish_list>
parseDishList :: Parser [Dish]
parseDishList input = ((and4 (\a b c d -> a ++ d) parseDish (parseCertainChar ',') parseWhitespace parseDishList) `or2` (parseDish)) input 

-- <dish> ::= <main_course> | <side_dish> | <beverage>
parseDish :: Parser [Dish]
parseDish input = listDish ((parseMainCourse `or2` parseSideDish `or2` parseBeverage) input)

-- helper function to put a dish to a list
listDish :: Either String (Dish, String) -> Either String ([Dish], String)
listDish dish = case dish of
  Right (v, rest) -> Right ([v], rest)
  Left e -> Left e

-- <digit> ::= [0-9]
parseDigit :: Parser Int
parseDigit [] = Left "<parseDigitError> empty input, cannot parse digit"
parseDigit (h:t) =
  if C.isDigit h then Right(read [h], t) else Left "<parseDigitError> Not a digit"

parseOrderHelper2 :: Order -> String -> Parser Order
parseOrderHelper2 order endOfOrder input =
  let
    order1 = case and2 (\_ b -> b) parseWhitespace parseTableNumber input of
      Right(v1, r1) -> order{tableNumber=Just v1}
      Left _ -> order
    input1 = case and2 (\_ b -> b) parseWhitespace parseTableNumber input of
      Right(v1, r1) -> r1
      Left _ -> input
  in
    let
      order2 = case and2 (\_ b -> b) parseWhitespace parsePaymentInfo input1 of
        Right(v2, r2) -> order1{paymentInfo=Just v2}
        Left _ -> order1
      input2 = case and2 (\_ b -> b) parseWhitespace parsePaymentInfo input1 of
        Right(v2, r2) -> r2
        Left _ -> input1
    in
      let
        order3 = case and2 (\_ b -> b) parseWhitespace parseTip input2 of
          Right(v3, r3) -> order2{tipAmount=Just v3}
          Left _ -> order2
        input3 = case and2 (\_ b -> b) parseWhitespace parseTip input2 of
          Right(v3, r3) -> r3
          Left _ -> input2
      in
        let
          editRes = and5 (\_ _ _ _ a -> a) (parseCertainNWords 2 "" " edit order") (parseCertainChar ':') parseWhitespace (parseCertainChar '[') parseWhitespace input3
          order4 = case editRes of
            Right(v4, r4) -> case parseOrder (endOfOrder ++ " ]") r4 of
              Right (editOrder, rest) ->
                case and2 (\_ b -> b) parseWhitespace (parseCertainChar ']') rest of
                  Right (_,rest) -> order3{editOrder=Just editOrder}
                  Left _ -> order3
              Left _ -> order3
            Left _ -> order3
          input4 = case editRes of
            Right(v4, r4) -> case parseOrder (endOfOrder ++ " ]") r4 of
              Right (editOrder, rest) ->
                case and2 (\_ b -> b) parseWhitespace (parseCertainChar ']') rest of
                  Right (_,rest) -> rest
                  Left _ -> "Expected ] at the end of edit order"
              Left e -> "Error parsing edit order: " ++ e
            Left e -> e
        in
          if input3 == "" then Right (order3, input3) else 
            if input4 == endOfOrder then Right(order4, input4) else Left input4

-- <order> ::= <command> " " <dish_list> (" " <table_number>)? (" " <payment_info>)? (" " <tip>)? (" edit order: [ " <order> " ]")?
parseOrder :: String -> Parser Order
parseOrder endOfOrder input = 
  let 
    mainOrder = (and3 (\a _ b -> (OrderObject a b Nothing Nothing Nothing Nothing)) parseCommand parseWhitespace parseDishList) input
  in
    case mainOrder of
      Right (v1, r2) -> 
        if r2 == endOfOrder then Right(v1, r2) else parseOrderHelper2 v1 endOfOrder r2
      Left e -> Left e
  
  





