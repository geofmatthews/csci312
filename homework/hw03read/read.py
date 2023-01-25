'''
Implementation of Scheme's read in Python
Geoffrey Matthews
2022
'''

import string

debug = False

def read(s):
    return read_sexpr(list(s))

punctuation = string.whitespace + '()'

def skip_blanks(s):
    while s[0] in string.whitespace:
        s.pop(0)

def read_sexpr(s):
    global debug
    if debug:
        print('sread:', ''.join(s))
    skip_blanks(s)
    if s[0] in string.digits:
        return read_number(s)
    elif s[0] not in punctuation:
        return read_symbol(s)
    elif s[0] == '(':
        s.pop(0)
        return read_list(s)

def read_number(s):
    global debug
    if debug:
        print('parse_number:', ''.join(s))
    skip_blanks(s)
    n = 0
    while s and s[0] in string.digits:
        n = n*10 + int(s.pop(0))
    return n

def read_symbol(s):
    global debug
    if debug:
        print('parse_symbol:', ''.join(s))
    skip_blanks(s)
    w = ''
    while s and s[0] not in punctuation:
        w += s.pop(0)
    return w

def read_list(s):
    global debug
    if debug:
        print('parse_list:', ''.join(s))
    skip_blanks(s)
    if s[0] == ')':
        s.pop(0)
        return []
    car = read_sexpr(s)
    cdr = read_list(s)
    return [car] + cdr

if __name__ == '__main__':
    for s in ['3425',
              'fubar',
              '+', '()',
              '((()))',
              '( + (* 13 44  ) 666)',
              '(let ((a 3) (b 4)) (+ a b))']:
        print(s, '=>', read(s))
        print('-'*60)
