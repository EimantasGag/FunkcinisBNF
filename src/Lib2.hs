{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where

import qualified Data.Char as C
import qualified Data.List as L

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query

-- | The instances are needed basically for tests
instance Eq Query where
  (==) _ _= False

instance Show Query where
  show _ = ""

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery _ = Left "Not implemented 2"

-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data State

-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = error "Not implemented 1"

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition _ _ = Left "Not implemented 3"

-- <order> ::= <command> " " <dish_list> (" " <table_number> | " " <payment_info> | " " <tip> | " edit order: [ " <order> " ]")* +

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

-- <order> ::= <command> " " <dish_list> (" " <table_number> | " " <payment_info> | " " <tip> | " edit order: [ " <order> " ]")*
data Order = OrderObject {
  command :: Command,
  dishList :: [Dish],
  tableNumber :: Maybe Int,
  paymentInfo :: Maybe PaymentInfo,
  tipAmount :: Maybe TipAmount,
  editOrder :: Maybe Order
} deriving Show

data Command = Order | Add | Remove
  deriving (Show, Eq)

data MainCourse = Pizza | Burger | Pasta | Salad | Steak | Sushi
  deriving (Show, Eq)

data SideDish = Fries | GarlicBread | Soup
  deriving (Show, Eq)

data Beverage = Cola | Water | Juice | Wine | Beer
  deriving (Show, Eq)

data Dish = MainCourse MainCourse | SideDish SideDish | Beverage Beverage
  deriving (Show, Eq) 

data TipAmount = TipAmount String
  deriving (Show, Eq)

data PaymentInfo = PayWithCard | PayWithCash
  deriving (Show, Eq)

parseWord :: Parser String
parseWord input =
    let letters = L.takeWhile C.isLetter input
        rest = L.drop (length letters) input
    in if not (null letters)
        then Right (letters, rest)
        else Left (input ++ " does not start with a letter")

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
      _ -> Left (word ++ " main course dish does not exist")
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
          _ -> Left (word ++ " side dish does not exist")
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
      _ -> Left (word ++ " beverage does not exist")
    Left e -> Left e

-- <command> ::= "Order" | "Add" | "Remove"
parseCommand :: Parser Command
parseCommand input = 
  case parseWord input of
    Right (word, rest) -> case word of
      "Order" -> Right (Order, rest)
      "Add" -> Right (Add, rest)
      "Remove" -> Right (Remove, rest)
      _ -> Left "Command does not exist"
    Left _ -> Left "Command does not exist"

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
parseCertainNWords 0 res match input = if match == res then Right (res, input) else Left (res ++ " does not match with " ++ match)
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
                Left e2 -> Left (e1 ++ ", " ++ e2)

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
parseTableNumber :: Parser Int
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

parseOrderHelper :: Order -> Parser Order
parseOrderHelper order " ]" = Right(order, " ]")
parseOrderHelper order "" = Right(order, "")
parseOrderHelper (OrderObject{command=c,dishList=d,tableNumber=t,paymentInfo=p,tipAmount=tip,editOrder=e}) input = 
  case and2 (\_ b -> b) parseWhitespace parseTableNumber input of 
    Right(v1, r1) -> parseOrderHelper OrderObject{command=c,dishList=d,tableNumber=Just v1,paymentInfo=p,tipAmount=tip,editOrder=e} r1
    Left _ ->
      case and2 (\_ b -> b) parseWhitespace parsePaymentInfo input of
        Right(v2, r2) -> parseOrderHelper OrderObject{command=c,dishList=d,tableNumber=t,paymentInfo=Just v2,tipAmount=tip,editOrder=e} r2
        Left _ -> 
          case and2 (\_ b -> b) parseWhitespace parseTip input of
            Right(v3, r3) -> parseOrderHelper OrderObject{command=c,dishList=d,tableNumber=t,paymentInfo=p,tipAmount=Just v3,editOrder=e} r3
            Left _ -> 
              case and5 (\_ _ _ _ a -> a) (parseCertainNWords 2 "" " edit order") (parseCertainChar ':') parseWhitespace (parseCertainChar '[') parseWhitespace input of
                Right(v4, r4) -> case parseOrder r4 of
                  Right (editOrder, rest) -> 
                    case and2 (\_ b -> b) parseWhitespace (parseCertainChar ']') rest of
                      Right(_,rest) -> Right (OrderObject{command=c,dishList=d,tableNumber=t,paymentInfo=p,tipAmount=tip,editOrder=Just editOrder}, rest)
                      Left _ -> Left "<parseOrderError> Expected ] at the end of edit order"
                  Left err -> Left err 
                Left _ -> Left ("<parseOrderError> Failed to parse, false syntax: " ++ input) 

-- <order> ::= <command> " " <dish_list> (" " <table_number> | " " <payment_info> | " " <tip> | " edit order: [ " <order> " ]")*
parseOrder :: Parser Order
parseOrder input = 
  let 
    mainOrder = (and3 (\a _ b -> (OrderObject a b Nothing Nothing Nothing Nothing)) parseCommand parseWhitespace parseDishList) input
  in
    case mainOrder of
      Right (v1, r2) -> parseOrderHelper v1 r2
      Left _ -> Left "<parseOrderError> Error parsing order"
  
  





