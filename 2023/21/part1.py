import os
from collections import defaultdict

GOAL = 64

grid = None
q = defaultdict(lambda: 0)
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    data = [list(x) for x in f]
    for i, x in enumerate(data):
        for j, y in enumerate(x):
            if y == 'S':
                q[(0, i, j)] += 1
                break
    grid = [[y != '#' for y in x] for x in data]

s = 0
while len(q) > 0:
    d, i, j= next(iter(q))
    a = q.pop((d, i, j))
    if d == GOAL:
        s += 1
        continue
    if i > 0 and grid[i - 1][j]:
        q[(d + 1, i - 1, j)] += a
    if i + 1 < len(grid) and grid[i + 1][j]:
        q[(d + 1, i + 1, j)] += a
    if j > 0 and grid[i][j - 1]:
        q[(d + 1, i, j - 1)] += a
    if j + 1 < len(grid[0]) and grid[i][j + 1]:
        q[(d + 1, i, j + 1)] += a

print(s)