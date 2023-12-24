import os
from sympy import Matrix

MIN, MAX = 7, 27
MIN, MAX = 200000000000000, 400000000000000

with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    functions = [tuple(int(c) for p in l.strip().split(' @ ') for i, c in enumerate(p.split(', ')) if i != 2) for l in f]

sm = 0
for i, [a1, a2, b1, b2] in enumerate(functions):
    print(f"{i + 1}/{len(functions)}")
    for [c1, c2, d1, d2] in functions[i+1:]:
        rref, pivo = Matrix([[-b1, d1, a1 - c1], [-b2, d2, a2 - c2]]).rref()
        if pivo != (0, 1): continue
        r, s = rref[0, 2], rref[1, 2]
        if r < 0 or s < 0: continue
        x1 = a1 + r * b1
        x2 = a2 + r * b2
        if x1 < MIN or x1 > MAX or x2 < MIN or x2 > MAX: continue
        sm += 1

print(sm)