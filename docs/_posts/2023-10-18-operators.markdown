---
layout: post
title:  "Logical operators"
date:   2023-10-23 15:22:07 +0200
categories: syntax
---

### In the GLaDOS language, there are several different data types. These include:

* `int` - An integer value
{% highlight BackusNaur %}
<int> ::= 'Int64 MinValue' .. 'Int64 MaxValue'
{% endhighlight %}
* `float` - A floating point value
{% highlight BackusNaur %}
<float> ::= 'Float64 MinValue' .. 'Float64 MaxValue'
{% endhighlight %}
* `char` - A single character
{% highlight BackusNaur %}
<char> ::= 'An ascii character'
{% endhighlight %}
* `string` - A string of characters terminated by a null character
{% highlight BackusNaur %}
<string> ::= 'An array of ascii characters' 'Null character'
{% endhighlight %}
* `bool` - A boolean value
{% highlight BackusNaur %}
<bool> ::= 'True' | 'False'
{% endhighlight %}
* `NULL` - A null value
{% highlight BackusNaur %}
<null> ::= 'Null'
{% endhighlight %}