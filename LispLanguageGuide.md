# GLaDOS - Lisp language usage guide
## Table of contents
- Basic syntax
- Default symbols

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

Returns the product of the two values

> `> (* 2 3)`  
`6`

- Division " / "

Returns the quotient of the first value divided by the second

> `> (/ 6 2)`  
`3`