---
layout: post
title:  "Conditional statements"
date:   2023-10-23 15:22:07 +0200
categories: syntax
---
### In the GLaDOS language, you can express a conditional statement in the following way:
{% highlight BackusNaur %}
<if-statement> ::= 'if' (<expression>) { <statement> }
{% endhighlight %}
The `if` keyword is followed by an expression, which is then followed by a block of statements. The block of statements is executed if the expression evaluates to `True`. If the expression evaluates to `False`, the block of statements is skipped.

You can also express an `if` statement with an `else` clause:
{% highlight BackusNaur %}
<if-statement> ::= 'if' (<expression>) { <statement> } 'else' { <statement> }
{% endhighlight %}
The `else` clause is executed if the expression evaluates to `False`.

Finaly, the `if` statement can be expressed with an `else if` clause:
{% highlight BackusNaur %}
<if-statement> ::= 'if' (<expression>) { <statement> } 'else if' (<expression>) { <statement> }
{% endhighlight %}

#### An expression must evaluate to a boolean value (True or False), if not, it will systematically evaluate to False.

#### Example:
{% highlight GLaDOS %}
if (1 == 1) {
    print("1 is equal to 1");
}
{% endhighlight %}

#### Output:
{% highlight bash %}
1 is equal to 1
{% endhighlight %}
