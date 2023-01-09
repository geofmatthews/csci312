'''
Implementation of Scheme's read in Python
Geoffrey Matthews
2022
'''

import string

debug = False

def read(s):
    return parse(list(s))

punctuation = string.whitespace + '()'


def skip_blanks(s):
    while s[0] in string.whitespace:
        s.pop(0)

def parse(s):
    global debug
    if debug:
        print('parse:', ''.join(s))
    skip_blanks(s)
    if s[0] in string.digits:
        return parse_number(s)
    elif s[0] not in punctuation:
        return parse_symbol(s)
    elif s[0] == '(':
        s.pop(0)
        return parse_list(s)

def parse_number(s):
    global debug
    if debug:
        print('parse_number:', ''.join(s))
    skip_blanks(s)
    n = 0
    while s and s[0] in string.digits:
        n = n*10 + int(s.pop(0))
    return n

def parse_symbol(s):
    global debug
    if debug:
        print('parse_symbol:', ''.join(s))
    skip_blanks(s)
    w = ''
    while s and s[0] not in punctuation:
        w += s.pop(0)
    return w

def parse_list(s):
    global debug
    if debug:
        print('parse_list:', ''.join(s))
    skip_blanks(s)
    if s[0] == ')':
        s.pop(0)
        return []
    car = parse(s)
    cdr = parse_list(s)
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
