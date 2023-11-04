---
layout: post
title:  "Syntax introduction"
date:   2023-10-23 15:22:07 +0200
categories: syntax
---
### In this part of the documentation, we will go through the general syntax of the GLaDOS language. This includes the following:
- Types
- Logical operators
- Arithmetic operators
- Conditional statements
- Loops
- Functions
- Comments

#### You will notice that our implementation of the GLaDOS language looks quite similar to the C programming language. Our language borrows part of the C programming language separators, keywords, etc... There is still some elements of it that display a clear difference to the C programming language, like the fact that variables are not typed, or that a lot of C's standard library are builtin functions in GLaDOS.

#### Here's the full BNF of the language

{% highlight GLaDOS %}
<syntax> ::= "" | <statement> <syntax>

<statement> ::= <item> ";" | <if-tree> | <while-statement> | <for-statement> | <function-definition>

<item> ::= <ternary> | <infix> | <unary> | <function-call> | <subscription> | <const> | <symbol> | "(" <item> ")"
<if-tree> ::= <if-statement> <elif-statement> <else-statement>
<while-statement> ::= "while" <condition> <code-block>
<for-statement> ::= "for" "(" <item> ";" <item> ";" <item> ")" <code-block>
<function-definition> ::= "unction" <symbol> "(" <comma-separated-symbol-list> ")" "{" () "}"

<code-block> = "{" <syntax> "}"
<condition> = <condition>

<if-statement> ::= "if" <condition> <code-block>
<elif-statement> ::= "" | "elif" <condition> <code-block>
<else-statement> ::= "" | "else" <code-block>

<function-statement> ::= "" | <syntax> | "return" <item> ";"
<comma-separated-symbol-list> ::= "" | <symbol> <comma-separated-symbol-list>

<ternary> ::= <item> "?" <item> ":" <item>

<infix> ::= <item> <infix-operator> <item>

<unary> ::= <unary-operator> <item>

<comma-separated-item-list> ::= "" | <item> <comma-separated-item-list>

<subscription> ::= <symbol> "." <function-call> | <symbol> "[" <item> "]"

<const> ::= <string> | <char> | <list> | <int> | <float> | <bool> | <nil>

<char> ::= "'" <ascii-char> "'"
<list> ::= "[" <comma-separated-item-list> "]"
<int> ::= "" | <digit> <int>
<float> ::= <int> "." <int>
<bool> ::= "True" | "False"
<nil> ::= "nil"
<string> ::= '"' <ascii-char> '"'
<inside-string> ::= "" | <ascii-char> <inside-string>

<symbol> ::= <head-symbol-char> <tail-symbol-char>
<head-symbol-char> ::= <letter>
<tail-symbol-char> ::= "" | (<head-symbol-char> | "-" | "_" | <digit>)
<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
<letter> ::= "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z"
<complex-char-no-apostrophes> ::= "|" | " " | "!" | "#" | "$" | "%" | "&" | "(" | ")" | "*" | "+" | "," | "-" | "." | "/" | ":" | ";" | ">" | "=" | "<" | "?" | "@" | "[" | "\" | "]" | "^" | "_" | "`" | "{" | "}" | "~"
<ascii-char> = <complex-char-no-apostrophes> | <letter> | <digit> | "\n" | "\t" | "\r"

<infix-operator> ::= "+" | "-" | "*" | "/" | "%" | "and" | "or" | "==" | "=" | "<=" | "<" | ">=" | ">" | "!="
<unary-operator> ::= "+" | "-" | "not"

<function-call> ::= <symbol> "(" <comma-separated-item-list> ")"
{% endhighlight %}
