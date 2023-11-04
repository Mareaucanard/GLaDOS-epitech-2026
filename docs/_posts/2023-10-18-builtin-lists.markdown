---
layout: post
title:  "lists"
date:   2023-10-23 15:22:07 +0200
categories: builtin
---

- `concat` - takes a list of strings or a list of lists and concatenates them into a single string or list.
{% highlight GLaDOS %}
concat(list);
{% endhighlight %}

- `head` - takes a list and returns the first element.
{% highlight GLaDOS %}
head(list);
{% endhighlight %}

- `tail` - takes a list and returns a list containing all elements except the first.
{% highlight GLaDOS %}
tail(list);
{% endhighlight %}

- `len` - takes a list and returns the number of elements.
{% highlight GLaDOS %}
len(list);
{% endhighlight %}

- `any` - takes a list of values, returns true if at least one value is a true boolean value.
{% highlight GLaDOS %}
any(list);
{% endhighlight %}

- `all` - takes a list of values, returns true if all values are true boolean values.
{% highlight GLaDOS %}
all(list);
{% endhighlight %}

- `prepend` - takes a list and a value, returns a list with the value prepended.
{% highlight GLaDOS %}
prepend(list, value);
{% endhighlight %}

- `map` - takes a list and a function, returns a list with the function applied to each element.
{% highlight GLaDOS %}
map(list, function);
{% endhighlight %}

- `range` - takes a integer, returns a list of integers from 0 to the integer.
{% highlight GLaDOS %}
range(integer);
{% endhighlight %}

- `nils` - takes an integer, returns a list of nils of the given length.
{% highlight GLaDOS %}
nils(integer);
{% endhighlight %}

- `append` - takes a list and a value, returns a list with the value appended.
{% highlight GLaDOS %}
append(list, value);
{% endhighlight %}
