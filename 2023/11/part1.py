import os
from itertools import combinations
from functools import reduce

universe = []

with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    for l in f:
        line = [c == '#' for c in l.rstrip()]
        universe.append(line)
        if not any(line): universe.append(line[:])

for i in range(len(universe[0]) - 1, -1, -1):
    if not any(x[i] for x in universe):
        for x in universe: x.insert(i, False)

galaxies = [(i, j) for i, x in enumerate(universe) for j, y in enumerate(x) if y]

def reduction(prev, curr):
    [[i1, j1], [i2, j2]] = curr
    return prev + abs(i1 - i2) + abs(j1 - j2)

print(reduce(reduction, combinations(galaxies, 2), 0))