# GLaDOS - Lisp language usage guide
## Table of contents
- [GLaDOS - Lisp language usage guide](#glados---lisp-language-usage-guide)
  - [Table of contents](#table-of-contents)
  - [Basic syntax](#basic-syntax)
  - [Default Symbols](#default-symbols)
    - [Arithmetic operations](#arithmetic-operations)
    - [Logical operations](#logical-operations)
    - [Binary operations](#binary-operations)

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
### Arithmetic operations

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

- Division " div "

Returns the quotient of the first value divided by the second.

> `> (div 6 2)`
`3`

- Modulo " mod "

Returns the modulo result of the first value by the second value.

> `> (mod 7 2)`
`1`

- Power " ** "

Returns the result of the value to the power of the second value.

> `> (** 2 3)`
`8`

### Logical operations

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

- Cons " cons "

Appends the second value to the first value (a list) and returns it.

> `> (cons [1 2] 3)`
`[1, 2, 3]`

- isEmpty " isemtpy "

Returns a boolean value depending on if the value is empty or not.

> `> (isempty [])`
`True`

- Empty " empty "

Empties the list and returns it.

> `> (empty [1, 2, 3])`
`[]`

- And " and "

Returns True if both values are True, returns false otherwise.

> `> (and #t #t)`
`True`

- Or " or "

Returns True if at least one of the two values is True, returns false otherwise.

> `> (or #t #f)`
`True`

- Not " not "

Returns the opposite of the given boolean value.

> `> (not #t)`
`False`

### Binary operations

- Binary And " & "

Takes two equal-length binary representations and performs the logical AND operation on each pair of the corresponding bits.
Returns the result of all the operations put in a single number.

> `> (& 12 27)`
`8`

- Binary Or " | "

Takes two equal-length binary representations and performs the logical OR operation on each pair of the corresponding bits.
Returns the result of all the operations put in a single number.

> `> (| 12 27)`
`31`

- Binary Xor " ^ "

Takes two equal-length binary representations and performs the logical XOR operation on each pair of the corresponding bits.
Returns the result of all the operations put in a single number.

> `> (^ 12 27)`
`23`

- Binary Not " ~ "

Takes two equal-length binary representations and performs the logical NOT operation on each pair of the corresponding bits.
Returns the result of all the operations put in a single number.

> `> (~ 12)`
`-13`

- Binary Left Shift " << "

Moves all the bits of the first value to the left, a number of times depending on the second value.

> `> (<< 12 3)`
`96`

- Binary Right Shift " >> "

Moves all the bits of the first value to the right, a number of times depending on the second value.

> `> (<< 12 3)`
`1`

- Nil " nil "

Returns Null or empty.

> `> nil`
` `

- Seed " seed "

Returns the seed.

> `> seed`
`8115863839095233`
