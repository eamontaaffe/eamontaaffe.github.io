---
title: Caching Property
---

Over the last couple of days, I have used a simple little pattern to cache long
running idempotent instance methods.

Initially I decided to use a property which overwrote itself when called the
first time.

```python
import time

class Foo():

    def long_function(self):
        time.sleep(5)
        return 5

    @property
    def result(self):
        self.result = long_function()
        return self.result

```

In this example, the first time `Foo.result` is called, it calculates the
desired value and binds it to the location of the property, then it returns
what is now the bound value.

Although it is a bit hacky re-assigning it's own value within the property
method, this pattern actually works pretty well. That is until you use it on an
object which inherits from `object`, which was an issue raised by my coworker,
David.

When inheriting from object, ie:

``` python
class Foo(object):
```

Assigning the value of the property within the method raises an
`AttributeError`.

## Better pattern
With the first option proven to be shotty, a better solution is to assign the
result to a private instance variable. Then rather than re-assigning the
property, you can just check if the variable exists before returning it's
result.

```python
import time

class Foo():

    def __init__(self):
        self._result = None

    def long_function(self):
        time.sleep(5)
        return 5

    @property
    def result(self):
        if not self._result:
            self._result = self.long_function()
        return self._result
```
