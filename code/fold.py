def fold(f, v, ls):
    if ls == []:
        return v
    else:
        return f(ls[0], fold(f, v, ls[1:]))

def add(a,b):
    return a + b

print(fold(add, 0, [1,2,3,4]))

def cons(a,b):
    return [a] + b

def product(xs, others):
    return fold(lambda item, tuples:
                fold(cons,
                     tuples,
                     fold(lambda x, ls: cons([item, x], ls),
                          [],
                          others)
                     ),
                [],
                xs)

print(product([1,2,3],[4,5,6]))

