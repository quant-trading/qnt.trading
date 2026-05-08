# import module responsible for time processing
from datetime import datetime
import functools


def timestamp(f):
    
    @functools.wraps(f)
    def wrapper(*args, **kwargs):
        # get current time using now() method
        timestamp = datetime.now()
        # convert timestamp to human-readable string, following passed pattern:
        string_timestamp = timestamp.strftime('%Y-%m-%d %H:%M:%S')
        print(string_timestamp)
        return f(*args, **kwargs)
    return wrapper


@timestamp
def simple_function(a, b):
    print(a + b)
    
    
simple_function(2, 2)