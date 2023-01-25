from ast import *
import string

def prune(tree):
    '''Remove unused branches from python parse tree'''
    if type(tree) is Module:
        return Module(body=prune(tree.body))
    elif type(tree) is list:
        return [prune(x) for x in tree]
    elif type(tree) is Name:
        return Name(id=tree.id)
    elif type(tree) is arg:
        return arg(arg=tree.arg)
    elif type(tree) is FunctionDef:
        return FunctionDef(name = tree.name,
                           args = prune(tree.args),
                           body = prune(tree.body))
    elif type(tree) is arguments:
        return arguments(args = prune(tree.args))
    elif type(tree) is Return:
        return Return(value=prune(tree.value))
    elif type(tree) is BinOp:
        return BinOp(left=prune(tree.left),
                     op=tree.op,
                     right=prune(tree.right))
    elif type(tree) is Expr:
        return Expr(value=prune(tree.value))
    elif type(tree) is Call:
        return Call(func=prune(tree.func),
                    args=prune(tree.args))
    elif type(tree) is Assign:
        return Assign(targets=prune(tree.targets),
                      value=prune(tree.value))
    else:
        return tree

def ppp(s, pruning=True):
    '''Prune, dump, and pretty-print'''
    print(s)
    if pruning:
        s = prune(parse(s))
    else:
        s = parse(s)
    ppd(s)

def ppd(p):
    pp(dump(prune(p)))

def pp(s):
    '''Pretty print a dumped Python parse tree'''
    def newln(indent):
        print('\n' + '  '*indent, end='')
    indent = 0
    for c in s:
        if c not in string.whitespace:
            print(c, end='')
        if c in '([':
            indent += 1
            newln(indent)
        if c in ')]':
            indent -= 1
            #newln(indent)
        if c in ',':
            newln(indent)
    print()
    print('-'*60)

if __name__ == '__main__':
    ppp('f(3+3+x)')
    
