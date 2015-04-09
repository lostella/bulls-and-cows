#!/usr/bin/python

if __name__ == '__main__':
    from utils import *
    from subprocess import *
    import sys
    import random
    ng = int(sys.argv[2])
    tot = 0
    m = 0
    perc = {}
    for k in range(ng):
        ans = random.choice(combos())
        process = Popen([sys.argv[1], ""], stdin = PIPE, stdout = PIPE)
        s = 0
        i = 0
        while s < 4:
            g = map(int, process.stdout.readline().strip(" \n").split(','))
            (s, b) = feedback(ans, g)
            process.stdin.write("%d,%d\n"%(s,b))
            process.stdin.flush()
            i = i+1
            if i > m:
                m = i
        tot = tot+i
        if not i in perc.keys():
            perc[i] = 1
        else:
            perc[i] += 1
        sys.stdout.write("%d/%d              \r"%(k+1,ng))
        sys.stdout.flush()
    for i in range(1,max(perc.keys())+1):
        if i in perc.keys():
            sys.stdout.write("%d: %.1f%%, "%(i, float(perc[i])/ng*100))
    sys.stdout.write("\n")
    print("avg: %.3f,"%(float(tot)/ng))
    print("max: %d."%(m))
