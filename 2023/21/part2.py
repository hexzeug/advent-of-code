import os

G = 26501365

with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    t = [0, 0]
    e = [0, 0]
    for i, r in enumerate(f):
        r = r.strip()
        if not i:
            l = len(r)
            n = (2 * G + 1) // l
            m = n // 2
            s = (l - 1) // 2
        for j, c in enumerate(r):
            if c == '#': continue
            g = (i ^ j) & 1
            t[g] += 1
            e[g] += abs(s - i) + abs(s - j) > s
    print(t[1] * (m+1)**2 + t[0] * m**2 - e[1] * (m+1) + (e[0]) * m)