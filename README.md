# CASINO DOMAIN

`<digit>` ::= `"0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"`

`<floating point>` ::= `<digit>.<digit>`

`<slotid>` ::= `<digit>` unique slot machine id

`<slotgame>` ::= `<text>` name of slot game

`<betamount>` ::= `<floating point>` bet amount in euros

`<gamble>` ::= `<slotid> <slotgame> <betamount>` function to gamble
