import os

TOP = 0b1000
RIGHT = 0b0100
BOTTOM = 0b0010
LEFT = 0b0001

TYPES = {
    '.': 0,
    'S': 0,
    '|': TOP | BOTTOM,
    '-': LEFT | RIGHT,
    'L': TOP | RIGHT,
    'J': TOP | LEFT,
    '7': BOTTOM | LEFT,
    'F': RIGHT | BOTTOM,
}

def top(p): return bool(p & TOP)
def right(p): return bool(p & RIGHT)
def bottom(p): return bool(p & BOTTOM)
def left(p): return bool(p & LEFT)

def inverse(d):
    return {
        TOP: BOTTOM,
        BOTTOM: TOP,
        LEFT: RIGHT,
        RIGHT: LEFT
    }[d]

pipes = []
si, sj = -1, -1
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    for i, l in enumerate(f):
        pipes.append([])
        for j, c in enumerate(l.rstrip()):
            pipes[-1].append(TYPES[c])
            if c == 'S': si, sj = i, j

pipes[si][sj] = (bottom(pipes[max(0, si - 1)][sj]) << 3) | (left(pipes[si][min(len(pipes[0]) - 1, sj + 1)]) << 2) | (top(pipes[min(len(pipes) - 1, si + 1)][sj]) << 1) | right(pipes[si][max(0, sj - 1)])

def look(i, j, d):
    return i - top(d) + bottom(d), j + right(d) - left(d)

coming_from = 0
i, j = si, sj
l = 0
while coming_from == 0 or i != si or j != sj:
    for d in [TOP, RIGHT, BOTTOM, LEFT]:
        if d != coming_from and pipes[i][j] & d:
            i, j = look(i, j, d)
            coming_from = inverse(d)
            break
    l += 1
print(l // 2)
