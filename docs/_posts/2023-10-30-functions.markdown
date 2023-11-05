---
layout: post
title:  "Functions definitions and expression"
date:   2023-10-23 15:22:07 +0200
categories: syntax
---
#### In GLaDOS, functions can be expressed as such:

{% highlight GLaDOS %}
function hello_world() {
    print("Hello World!");
}
{% endhighlight %}

Here, the `function` keyword is followed by the name of the function, then by a list of parameters between parentheses (none in this case), and finally by a block of statements.

{% highlight GLaDOS %}
function is_prime(x) {
    if (typeOf(x) != "integer" or x <= 1) {
        return False;
    }
    end = int(sqrt(x)) + 1;

    for (n = 2; n < end; n = n + 1) {
        if (x % n == 0) {
            return False;
        }
    }
    return True;
}
{% endhighlight %}

This function is here to highlight the use of an argument in a function. The `is_prime` function takes an argument `x` and returns `True` if `x` is a prime number, `False` otherwise.