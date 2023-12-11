import os
from itertools import combinations
from functools import reduce

MIO = 1_000_000

universe = []

with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    for l in f:
        line = [c == '#' for c in l.rstrip()]
        if not any(line): universe.append(['v'] * len(line))
        else: universe.append(line)

for i in range(len(universe[0]) - 1, -1, -1):
    if not any(x[i] == True for x in universe):
        for x in universe:
            if x[0] != 'v': x[i] = 'h'

vertical_coords = [0]
for x in universe[:-1]:
    vertical_coords.append(vertical_coords[-1] + (MIO if x[0] == 'v' else 1))

horizontal_coords = [0]
for x in universe:
    if x[0] == 'v': continue
    for y in x[:-1]:
        horizontal_coords.append(horizontal_coords[-1] + (MIO if y == 'h' else 1))
    break

galaxies = [(vertical_coords[i], horizontal_coords[j]) for i, x in enumerate(universe) for j, y in enumerate(x) if y == True]

def reduction(prev, curr):
    (i1, j1), (i2, j2) = curr
    return prev + abs(i1 - i2) + abs(j1 - j2)

print(reduce(reduction, combinations(galaxies, 2), 0))