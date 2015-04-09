def feedback(a, g):
    s = 0
    b = 0
    for i in range(4):
        if a[i] == g[i]:
            s = s+1
    for i in range(4):
        if a[i] in g and not a[i] == g[i]:
            b = b+1
    return (s, b)

def combos():
    ns = range(10)
    return [(n1,n2,n3,n4) for n1 in ns for n2 in ns\
                for n3 in ns for n4 in ns\
                if not (n1 == n2 or n1 == n3 or n1 == n4\
                or n2 == n3 or n2 == n4 or n3 == n4)]

def answers():
    return [(0,1),(0,2),(1,1),(1,0),(0,0),\
                (2,0),(0,3),(1,2),(2,1),(3,0),\
                (0,4),(1,3),(2,2)]
