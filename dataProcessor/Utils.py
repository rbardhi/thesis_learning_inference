'''
Created on May 25, 2018

@author: nitesh
'''

## 1 ) Removes brackets and special characters 
## 2 ) Convert int, float, decimal to string
def formInertString(val):
    if isinstance(val, basestring):
        val = val.lower()
    val = str(val)
    return val