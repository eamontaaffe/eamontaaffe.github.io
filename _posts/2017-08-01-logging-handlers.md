---
layout: post
---

Python's [logging module](https://docs.python.org/2/library/logging.html) is
pretty cool. A lot of large project's use it, suck as the
[Spotify's Luigi](https://github.com/spotify/luigi) project. By using the
logging module, we can keep all of the logging specific module in one place.
This means that is really easy to print to the command line in development and
easily switch to an external logging service when you move to production.

The logging module allows you to insert handlers, which all have a chance to
monitor the logging stream. If you were using an external service, then this is
where a simple http post request or similar would happen.

In this article, I am going to show you how to write the simplest version of
a handler possible. It's really easy and extremely useful.

``` python
import logging

class SimpleHandler(logging.Handler):
    def emit(self, record):
        # Do something exciting
        somethingExciting(record.msg)


handler = SimpleHandler()

logger.addHandler(handler)

# Now the logger is setup. Let's test it out!
logger.debug('debug message')
logger.info('info message')
time.sleep(60)
logger.warn('warn message')
logger.error('error message')
logger.critical('critical message')
```

That's it. To define a handler, we just need to extend the logging.Handler
class. Within this class, just provide your own version of the emit method.
This method receives each logging `record` as the user submits it to the logger
stream.
