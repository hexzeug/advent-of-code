import os
from statistics import mode

grid = None
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    grid = [[y for y in x.strip()] for x in f]

beams_directions = [[set() for _ in x] for x in grid]
beams = []
for i in range(len(grid)):
    beams.append((complex(0, -i), 1+0j, i))
    beams.append((complex(len(grid[0]) - 1, -i), -1+0j, i + len(grid)))
for i in range(len(grid[0])):
    beams.append((complex(i, 0), 0-1j, 2 * len(grid) + i))
    beams.append((complex(i, 1 - len(grid)), 0+1j, 2 * len(grid) + len(grid[0]) + i))

while len(beams) > 0:
    for i, [p, d, t] in reversed(list(enumerate(beams))):
        b_i, b_j = int(-p.imag), int(p.real)
        if b_i not in range(len(grid)) or b_j not in range(len(grid[0])) or (d, t) in beams_directions[b_i][b_j]:
            beams.pop(i)
            continue
        beams_directions[b_i][b_j].add((d, t))
        c = grid[b_i][b_j]
        if c == '-' and d.real == 0 or c == '|' and d.imag == 0:
            d *= 1j
            beams.append((p - d, -d, t))
        elif c == '/':
            d = complex(d.imag, d.real)
        elif c == '\\':
            d = -complex(d.imag, d.real)
        beams[i] = (p + d, d, t)

flat_dirs = [set(t for _, t in y) for x in beams_directions for y in x if len(y) > 0]
winning_type = mode(t for x in flat_dirs for t in x)
energized = 0

print(len([None for x in flat_dirs if winning_type in x]))