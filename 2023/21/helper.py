import os

G = 26501365

with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    data = [list(x.strip()) for x in f]
    l = len(data[0])
    n = (2 * G + 1) // l
    m = n // 2
    s = (l - 1) // 2
    o = s ^ G
    print(f"l: {l}, l/2: {l // 2}, n: {n}, m: {m}, o: {o & 1}")

import os
from collections import defaultdict

GOAL = 65

grid = None
q = defaultdict(lambda: 0)
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    data = [list(x.strip()) for x in f]
    for i, x in enumerate(data):
        for j, y in enumerate(x):
            if y == 'S':
                q[(0, i, j, 'O')] += 1
                break
    grid = [[y != '#' for y in x] for x in data]

q[(1, 0, 0, '|')] = 1
q[(1, 0, len(data[0]) - 1, '-')] = 1
q[(1, len(data) - 1, 0, '-')] = 1
q[(1, len(data) - 1, len(data[0]) - 1, '|')] = 1

s = 0
while len(q) > 0:
    d, i, j, t = next(iter(q))
    a = q.pop((d, i, j, t))
    data[i][j] = f"\033[91m{t}\033[0m" if (i ^ j) & 1 else f"\033[92m{t}\033[0m"
    if d == GOAL:
        s += 1
        continue
    if i > 0 and grid[i - 1][j]:
        q[(d + 1, i - 1, j, t)] += a
    if i + 1 < len(grid) and grid[i + 1][j]:
        q[(d + 1, i + 1, j, t)] += a
    if j > 0 and grid[i][j - 1]:
        q[(d + 1, i, j - 1, t)] += a
    if j + 1 < len(grid[0]) and grid[i][j + 1]:
        q[(d + 1, i, j + 1, t)] += a

print('\n'.join(''.join(x) for x in data))
print(s)