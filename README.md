# RESTAURANT DOMAIN

## **Main entities:**

`<order>` - Represents a customer's request to place and order for one or more dishes, to cancel an order or to add some dishes to already placed order (Recursive: Allows creating an order with an order to make some changes to the main order).

`<dish_list>` - Refers to a specific food item ordered by a customer. Dishes can be of different types such as main courses, side dishes, and beverages (Recursive: Allows listing dish_list containing dish_list).

## **Main operations:**

- Order one or more dishes
- Add dishes to an already placed order
- Remove dishes from an order

## **Order examples:**

- `Order Pizza,Burger,Fries,Water for table 12 pay with card tip 5$`
- `Add Salad for table 12`
- `Remove Pizza,Burger,Fries,Water,Salad for table 12`
- `Order Pizza, Pasta, Salad pay with card for table 1 edit order: [ Add Wine pay with cash edit order: [ Remove Salad ] ]` (Recursion example)

## **BNF Grammar for a Restaurant Ordering System: [BNF Playground link](https://bnfplayground.pauliankline.com/?bnf=<order>%20%3A%3A%3D%20<command>%20"%20"%20<dish_list>%20("%20"%20<table_number>%20%7C%20"%20"%20<payment_info>%20%7C%20"%20"%20<tip>%20%7C%20"%20edit%20order%3A%20%5B%20"%20<order>%20"%20%5D")*%0A<command>%20%3A%3A%3D%20"Order"%20%7C%20"Add"%20%7C%20"Remove"%0A<dish_list>%20%3A%3A%3D%20<dish>%20%7C%20<dish>%20"%2C%20"%20<dish_list>%0A<dish>%20%3A%3A%3D%20<main_course>%20%7C%20<side_dish>%20%7C%20<beverage>%0A<main_course>%20%3A%3A%3D%20"Pizza"%20%7C%20"Burger"%20%7C%20"Pasta"%20%7C%20"Salad"%20%7C%20"Steak"%20%7C%20"Sushi"%0A<side_dish>%20%3A%3A%3D%20"Fries"%20%7C%20"Garlic%20Bread"%20%7C%20"Salad"%20%7C%20"Soup"%0A<beverage>%20%3A%3A%3D%20"Cola"%20%7C%20"Water"%20%7C%20"Juice"%20%7C%20"Wine"%20%7C%20"Beer"%0A<table_number>%20%3A%3A%3D%20"for%20table%20"%20<number>%0A<tip>%20%3A%3A%3D%20"tip%20"%20<amount>%0A<amount>%20%3A%3A%3D%20%5B0-9%5D%2B%20("."%20%5B0-9%5D%20%5B0-9%5D)*%20"%24"%0A<number>%20%3A%3A%3D%20<digit>%20%7C%20<digit>%20<digit>%0A<digit>%20%3A%3A%3D%20%5B0-9%5D%0A<payment_info>%20%3A%3A%3D%20"pay%20with%20card"%20%7C%20"pay%20with%20cash"&name=)** 

`<order> ::= <command> " " <dish_list> (" " <table_number>)? (" " <payment_info>)? (" " <tip>)? (" edit order: [ " <order> " ]")?`

`<command> ::= "Order" | "Add" | "Remove"`

`<dish_list> ::= <dish> | <dish> ", " <dish_list>`

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
