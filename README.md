# CASINO DOMAIN

`<digit>` ::= `"0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"`

`<floating_point>`::= `<digit> "." <digit>`

`<text>` ::= `[a-Z]+`

`<slotid>` ::= `<digit>`

`<slotgame>` ::= `<text>`

`<betamount>` ::= `<floating_point>` 

`<gamble>` ::= `"gamble" <slotid> <slotgame> <betamount>` 
