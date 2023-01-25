from ast import *
from ast_utilities import *

s1 = '3+x'
s2 = '''
def f(x):
    return x + 1
print(f(3))
'''
s3 = '''
def f(a,b,c):
  x = a + b + c
  return 2*x
x = 2**4
print(f(3*x,2,z))
'''


for s in (s1,s2,s3):
    ppp(s,False)
    ppp(s)
    
    
