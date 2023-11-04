---
layout: post
title:  "Lists and strings"
date:   2023-10-23 15:22:07 +0200
categories: builtin
---

### Lists

- `concat` - takes a list of strings or a list of lists and concatenates them into a single string or list.
{% highlight GLaDOS %}
concat(list);
{% endhighlight %}
{% highlight GLaDOS %}
items = ["Hello", "World"]
concat(list); => "HelloWorld"
{% endhighlight %}

- `head` - takes a list and returns the first element.
{% highlight GLaDOS %}
head([0, 1, 2]); => 0
{% endhighlight %}

- `tail` - takes a list and returns a list containing all elements except the first.
{% highlight GLaDOS %}
tail(list);
tail([0, 1, 2]); => [1, 2]
{% endhighlight %}

- `len` - takes a list and returns the number of elements.
{% highlight GLaDOS %}
len(list);
{% endhighlight %}
{% highlight GLaDOS %}
len([0, 1, 2]); => 3
{% endhighlight %}

- `any` - takes a list of values, returns true if at least one value is a true boolean value.
{% highlight GLaDOS %}
any(list);
{% endhighlight %}
{% highlight GLaDOS %}
any([True, False]); => True
any([False, 1]) => False
any([]) => False
{% endhighlight %}

- `all` - takes a list of values, returns true if all values are true boolean values.
{% highlight GLaDOS %}
all(list);
{% endhighlight %}
{% highlight GLaDOS %}
all([True, True]); => True
all([True, 1]) => False
all([]) => True
{% endhighlight %}

- `append` - takes a list and a value, returns a list with the value appended.
{% highlight GLaDOS %}
append(list, value);
{% endhighlight %}
{% highlight GLaDOS %}
append([0, 1, 2], 3); => [0, 1, 2, 3]
{% endhighlight %}

- `prepend` - takes a list and a value, returns a list with the value prepended.
{% highlight GLaDOS %}
prepend(list, value);
{% endhighlight %}
{% highlight GLaDOS %}
prepend([0, 1, 2], -1); => [-1, 0, 1, 2]
{% endhighlight %}

- `map` - takes a list and a function, returns a list with the function applied to each element.
{% highlight GLaDOS %}
map(list, function);
{% endhighlight %}
{% highlight GLaDOS %}
function double(x) { return x * 2; }
map([0, 1, 2, 3, 4], double); => [0, 2, 4, 6, 8]
{% endhighlight %}

- `range` - takes a integer, returns a list of integers from 0 to the integer.
{% highlight GLaDOS %}
range(integer);
{% endhighlight %}
{% highlight GLaDOS %}
range(5); => [0, 1, 2, 3, 4]
{% endhighlight %}

- `nils` - takes an integer, returns a list of nils of the given length.
{% highlight GLaDOS %}
nils(integer);
{% endhighlight %}
{% highlight GLaDOS %}
nils(3); => [nils, nils, nils]
{% endhighlight %}

### Strings

- `reverse` - takes a string and returns a string with the characters in reverse order.
{% highlight GLaDOS %}
reverse(string);
{% endhighlight %}
{% highlight GLaDOS %}
reverse("Hello"); => "olleH"
{% endhighlight %}


- `split` - takes a string and a separator, returns a list of strings.
{% highlight GLaDOS %}
split(string, separator);
{% endhighlight %}
{% highlight GLaDOS %}
split("Hello world", ' '); => ["Hello", "world"]
split("Hello world", "ll"); => ["He", "o world"]
{% endhighlight %}

- `join` - takes a list of strings and a separator, returns a string.
{% highlight GLaDOS %}
join(separator, list);
{% endhighlight %}
{% highlight GLaDOS %}
join(" ", ["Hello", "world"]); => "Hello world"
{% endhighlight %}
