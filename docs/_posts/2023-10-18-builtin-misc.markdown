---
layout: post
title:  "misc"
date:   2023-10-23 15:22:07 +0200
categories: builtin
---

- `int` - takes a value, casts it as an integer
{% highlight GLaDOS %}
int(value);
{% endhighlight %}

- `float` - takes a value, casts it as a float
{% highlight GLaDOS %}
float(value);
{% endhighlight %}

- `str` - takes a value, casts it as a string
{% highlight GLaDOS %}
str(value);
{% endhighlight %}

- `throw` - takes a string, throws an error with the string as the message
{% highlight GLaDOS %}
throw(string);
{% endhighlight %}

- `vargs` - not a function, just a list of argument, doesn't include prog name
{% highlight GLaDOS %}
vargs;
{% endhighlight %}

- `rand` - returns a random float between 0 and 1
{% highlight GLaDOS %}
rand();
{% endhighlight %}

- `uniform` - takes two floats, returns a random float between them
{% highlight GLaDOS %}
uniform(min, max);
{% endhighlight %}

- `randrange` - takes two integers, returns a random integer between them
{% highlight GLaDOS %}
randrange(min, max);
{% endhighlight %}

- `time` - returns an epoch timestamp
{% highlight GLaDOS %}
time();
{% endhighlight %}

- `timeit` - returns a list with the current seconds and nanoseconds
{% highlight GLaDOS %}
timeit();
{% endhighlight %}

- `sleep` - takes a float, sleeps for that many seconds
{% highlight GLaDOS %}
sleep(float);
{% endhighlight %}

- `typeof` - takes a value, returns a string with the type
{% highlight GLaDOS %}
typeof(value);
{% endhighlight %}
