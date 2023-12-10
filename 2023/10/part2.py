import os

TOP = 0b1000
RIGHT = 0b0100
BOTTOM = 0b0010
LEFT = 0b0001

DIRS = [TOP, RIGHT, BOTTOM, LEFT]

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

loop = [[0] * len(x) for x in pipes]

right_side = []

def is_in_range(i, j):
    return i in range(len(loop)) and j in range(len(loop[0]))

coming_from = None
i, j = si, sj
while coming_from is None or i != si or j != sj:
    loop[i][j] = 1
    if (i, j) in right_side: right_side.remove((i, j))

    pipe = pipes[i][j]
    d_idx, d = [(i, d) for i, d in enumerate(DIRS) if d != coming_from and pipe & d][0]

    for off in range(1, 3):
        d_r = DIRS[(d_idx + off) % 4]
        if pipe & d_r: break
        ri, rj = look(i, j, d_r)
        if is_in_range(ri, rj) and loop[ri][rj] == 0:
            right_side.append((ri, rj))
            loop[ri][rj] = 2

    i, j = look(i, j, d)
    coming_from = inverse(d)

solution_is_right = True

itr = 0
while itr < len(right_side):
    i, j = right_side[itr]
    for d in DIRS:
        di, dj = look(i, j, d)
        if di not in range(len(loop)) or dj not in range(len(loop[0])):
            solution_is_right = False
            continue
        if loop[di][dj] == 0:
            right_side.append((di, dj))
            loop[di][dj] = 2
    itr += 1

print(sum(x.count(solution_is_right * 2) for x in loop))
