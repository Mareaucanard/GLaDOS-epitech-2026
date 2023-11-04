---
layout: post
title:  "IO"
date:   2023-10-23 15:22:07 +0200
categories: builtin
---

- `write` - Write `data` to it's `fd` passed as argument.
{% highlight GLaDOS %}
write(fd, data);
{% endhighlight %}

- `print` - Print `data` to the standard output with a newline.
{% highlight GLaDOS %}
print(data);
{% endhighlight %}

- `read` - Read from `fd` passed as argument untill a newline is read.
{% highlight GLaDOS %}
read(fd);
{% endhighlight %}

- `input` - Read from the standard input untill a newline is read.
{% highlight GLaDOS %}
input();
{% endhighlight %}

- `open` - Open a file at `path` with `flags` passed as argument.
{% highlight GLaDOS %}
open(path, flags);
{% endhighlight %}

- `close` - Close a file at `fd` passed as argument.
{% highlight GLaDOS %}
close(fd);
{% endhighlight %}

- `fetch` - Does a get on an `url` passed as argument.
{% highlight GLaDOS %}
fetch(url);
{% endhighlight %}
