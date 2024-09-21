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

## **BNF Grammar for a Restaurant Ordering System: [BNF Playground link](https://bnfplayground.pauliankline.com/?bnf=%3Corder%3E%20%3A%3A%3D%20%3Ccommand%3E%20%22%20%22%20%3Cdish_list%3E%20(%22%20%22%20%3Ctable_number%3E%20%7C%20%22%20%22%20%3Cpayment_info%3E%20%7C%20%22%20%22%20%3Ctip%3E)*%0A%3Ccommand%3E%20%3A%3A%3D%20%22Order%22%20%7C%20%22Add%22%20%7C%20%22Cancel%22%0A%3Cdish_list%3E%20%3A%3A%3D%20%3Cdish%3E%20%7C%20%3Cdish%3E%20%22%2C%22%20%3Cdish_list%3E%0A%3Cdish%3E%20%3A%3A%3D%20%3Cmain_course%3E%20%7C%20%3Cside_dish%3E%20%7C%20%3Cbeverage%3E%0A%3Cmain_course%3E%20%3A%3A%3D%20%22Pizza%22%20%7C%20%22Burger%22%20%7C%20%22Pasta%22%20%7C%20%22Salad%22%20%7C%20%22Steak%22%20%7C%20%22Sushi%22%0A%3Cside_dish%3E%20%3A%3A%3D%20%22Fries%22%20%7C%20%22Garlic%20Bread%22%20%7C%20%22Salad%22%20%7C%20%22Soup%22%0A%3Cbeverage%3E%20%3A%3A%3D%20%22Cola%22%20%7C%20%22Water%22%20%7C%20%22Juice%22%20%7C%20%22Wine%22%20%7C%20%22Beer%22%0A%3Ctable_number%3E%20%3A%3A%3D%20%22for%20table%20%22%20%3Cnumber%3E%0A%3Ctip%3E%20%3A%3A%3D%20%22tip%20%22%20%3Camount%3E%0A%3Camount%3E%20%3A%3A%3D%20%5B0-9%5D%2B%20(%22.%22%20%5B0-9%5D%20%5B0-9%5D)*%20%22%24%22%0A%3Cnumber%3E%20%3A%3A%3D%20%3Cdigit%3E%20%7C%20%3Cdigit%3E%20%3Cdigit%3E%0A%3Cdigit%3E%20%3A%3A%3D%20%5B0-9%5D%0A%3Cpayment_info%3E%20%3A%3A%3D%20%22pay%20with%20card%22%20%7C%20%22pay%20with%20cash%22&name=)** 

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
