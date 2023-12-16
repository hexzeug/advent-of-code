import os

grid = None
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    grid = [[y for y in x.strip()] for x in f]

beams_directions = [[set() for _ in x] for x in grid]
beams = [(0+0j, 1+0j)]

while len(beams) > 0:
    for i, [p, d] in reversed(list(enumerate(beams))):
        b_i, b_j = int(-p.imag), int(p.real)
        if b_i not in range(len(grid)) or b_j not in range(len(grid[0])) or d in beams_directions[b_i][b_j]:
            beams.pop(i)
            continue
        beams_directions[b_i][b_j].add(d)
        c = grid[b_i][b_j]
        if c == '-' and d.real == 0 or c == '|' and d.imag == 0:
            d *= 1j
            beams.append((p - d, -d))
        elif c == '/':
            d = complex(d.imag, d.real)
        elif c == '\\':
            d = -complex(d.imag, d.real)
        beams[i] = (p + d, d)

print(len([None for x in beams_directions for y in x if len(y) > 0]))