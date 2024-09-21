# RESTAURANT DOMAIN

## **Main entities:**

`<order>` - Represents a customer's request to place and order for one or more dishes, to cancel an order or to add some dishes to already placed order.

`<dish_list>` - Refers to a specific food item ordered by a customer. Dishes can be of different types such as main courses, side dishes, and beverages (Recursive: Allows listing multiple dishes).

## **Main operations:**

- Order one or more dishes
- Add dishes to an already placed order
- Cancel an order

## **Order examples:**

- `Order Pizza,Burger,Fries,Water for table 12 pay with card tip 5$`
- `Add Salad for table 12`
- `Cancel Pizza,Burger,Fries,Water,Salad for table 12`

## **BNF Grammar for a Restaurant Ordering System:** 

`<order> ::= <command> " " <dish_list> (" " <table_number> | " " <payment_info> | " " <tip>)*`

`<command> ::= "Order" | "Add" | "Cancel"`

`<dish_list> ::= <dish> | <dish> "," <dish_list>`

`<dish> ::= <main_course> | <side_dish> | <beverage>`

`<main_course> ::= "Pizza" | "Burger" | "Pasta" | "Salad" | "Steak" | "Sushi"`

`<side_dish> ::= "Fries" | "Garlic Bread" | "Salad" | "Soup"`

`<beverage> ::= "Cola" | "Water" | "Juice" | "Wine" | "Beer"`

`<table_number> ::= "for table " <number>`

`<tip> ::= "tip " <amount>`

`<amount> ::= [0-9]+ ("." [0-9] [0-9])* "$"`

`<number> ::= <digit> | <digit> <digit>`

`<digit> ::= [0-9]`

`<payment_info> ::= "pay with card" | "pay with cash"`
