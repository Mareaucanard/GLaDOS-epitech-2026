---
layout: post
title:  "Misc"
date:   2023-10-23 15:22:07 +0200
categories: builtin
---

- `int` - takes a value, casts it as an integer
{% highlight GLaDOS %}
int(value);
{% endhighlight %}
{% highlight GLaDOS %}
int("1") + int("1"); => 2
{% endhighlight %}

- `float` - takes a value, casts it as a float
{% highlight GLaDOS %}
float(value);
{% endhighlight %}
{% highlight GLaDOS %}
float("1.2") + float("1.7"); => 2.9
{% endhighlight %}

- `str` - takes a value, casts it as a string
{% highlight GLaDOS %}
str(value);
{% endhighlight %}
{% highlight GLaDOS %}
"The number is " + str(1) + "."; => "The number is 1."
{% endhighlight %}

- `throw` - takes a string, throws an error with the string as the message
{% highlight GLaDOS %}
throw(string);
{% endhighlight %}

- `vargs` - not a function, just a list of argument, doesn't include prog name
{% highlight GLaDOS %}
vargs;
{% endhighlight %}
{% highlight GLaDOS %}
./glados --exec file.gvm -- hello world
vargs; => ["hello", "world"]
{% endhighlight %}


- `rand` - returns a random float between 0 and 1
{% highlight GLaDOS %}
rand();
{% endhighlight %}
{% highlight GLaDOS %}
rand(); => 0.3787218973
{% endhighlight %}

- `uniform` - takes two floats, returns a random float between them
{% highlight GLaDOS %}
uniform(min, max);
{% endhighlight %}
{% highlight GLaDOS %}
uniform(10, 100); => 49.287491278
{% endhighlight %}


- `randrange` - takes two integers, returns a random integer between them
{% highlight GLaDOS %}
randrange(min, max);
{% endhighlight %}
{% highlight GLaDOS %}
randrange(10, 15); => 12
{% endhighlight %}

- `time` - returns an epoch timestamp
{% highlight GLaDOS %}
time();
{% endhighlight %}
{% highlight GLaDOS %}
time(); => 1699135732
{% endhighlight %}

- `timeit` - returns a list with the current seconds and nanoseconds
{% highlight GLaDOS %}
timeit();
{% endhighlight %}
{% highlight GLaDOS %}
timeit(); => [1699135732, 1283457]
{% endhighlight %}

- `sleep` - takes a float, sleeps for that many seconds
{% highlight GLaDOS %}
sleep(float);
{% endhighlight %}

- `typeof` - takes a value, returns a string with the type
{% highlight GLaDOS %}
typeof(value);
{% endhighlight %}
{% highlight GLaDOS %}
typeof(14); => "integer"
typeof(0.0); => "float"
typeof('h'); => "char"
typeof(True); => "boolean"
typeof("hello"); => "string"
typeof(nil); => "Nil"
typeof([]); => "list"
{% endhighlight %}
