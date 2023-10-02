# GLaDOS - Lisp language usage guide
## Table of contents
- Basic syntax
- Default symbols
  - Arithmetic operations
  - Logical operations
  - Boolean operations

## Basic syntax

An expression takes the form of symbol first, then first value, and finally the second value, like so:

>`> (+ 1 2)`  
`3`  
`> (/ (+ 4 2) 2)`  
`3`

Here, in the first expression, the operator '+' is applied to 1 and 2, adding 2 to 1.  
In the second expression, the operator '+' is applied first to 4 and 2, then the operator '/' is applied to the result of the first operation, and 2.
Thus, dividing 6 by 2.

Any expression should be put between parenthesis.

## Default Symbols

- Addition " + "

Returns the sum of the two values.
> `> (+ 2 1)`  
`3`

- Substraction " - "

Returns the difference of the two values.
> `> (- 2 1)`  
`1`

- Multiplication " * "

Returns the product of the two values.

> `> (* 2 3)`  
`6`

- Division " / "

Returns the quotient of the first value divided by the second.

> `> (/ 6 2)`  
`3`

- Modulo " % "

Returns the modulo result of the first value by the second value.

> `> (% 7 2)`  
`1`

- Power " ^ "

Returns the result of the value to the power of the second value.

> `> (^ 2 3)`  
`8`

- if " if "

Returns a boolean value depending on the condition given as argument.

> `> (if #t)`  
`True`

- Equivalent " eq? " or " == "

Returns a boolean value depending if both values given are equal.

> `> (== #t #t)`  
`True`

- Equivalent " != " or "/= " or " =/ "

Returns a boolean value depending if given values are different.

> `> (== #t #f)`  
`True`

- Inferior " < "

Returns a boolean value depending on if the first value is inferior to the second.

> `> (< 3 5)`  
`True`

- Superior " > "

Returns a boolean value depending on if the first value is superior to the second.

> `> (> 3 5)`  
`False`

- Inferior or equal " <= "

Returns a boolean value depending on if the first value is inferior or equal to the second.

> `> (<= 5 5)`  
`True`

- Superior or equal " >= "

Returns a boolean value depending on if the first value is superior or equal to the second.

> `> (>= 5 5)`  
`True`

- List " list "

Returns a list created with the given values.

> `> (list 1 2 3)`  
`[1, 2, 3]`

- Car " car "

Returns the head of the given list.

> `> (car 1 2 3)`  
`1`

- Cdr " cdr "

Returns the tail of the given list.

> `> (cdr 1 2 3)`  
`[2, 3]`

