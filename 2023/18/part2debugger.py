import os
os.system('color')

def CALC_FAKE_MAP():
    instr = []
    min_l, max_r, min_t, max_b = 0, 0, 0, 0
    lr, tb = 0, 0
    with open(f"{os.path.dirname(__file__)}/input.txt") as f:
        for l in f:
            r, d, _ = l.split()
            d = int(d)
            if r == 'R':
                lr += d
                max_r = max(max_r, lr)
                instr.append((d, 0))
            elif r == 'L':
                lr -= d
                min_l = min(min_l, lr)
                instr.append((-d, 0))
            elif r == 'D':
                tb += d
                max_b = max(max_b, tb)
                instr.append((0, d))
            elif r == 'U':
                tb -= d
                min_t = min(min_t, tb)
                instr.append((0, -d))
            
    mp = [['.'] * (max_r - min_l + 1) for _ in range(max_b - min_t + 1)]

    x, y = -min_l, -min_t
    for dx, dy in instr:
        for i in range(min(y, y + dy), max(y, y + dy) + 1):
            for j in range(min(x, x + dx), max(x, x + dx) + 1):
                mp[i][j] = '#'
        x += dx
        y += dy
    return mp, min_l, min_t

FAKE_MAP, MIN_X, MIN_Y = CALC_FAKE_MAP()
SEARCH = 92

i = 0
def PRINT_MAP(lines):
    global i
    if SEARCH != -1:
        print(i)
        if i == SEARCH:
            pass
        i += 1
        return
    crash = False
    for y, row in enumerate(FAKE_MAP):
        for x, char in enumerate(row):
            in_line = None
            for l in lines:
                if y + MIN_Y == l.y:
                    before = in_line
                    if x + MIN_X == l.x1: in_line = '\033[92m<\033[0m'
                    elif x + MIN_X == l.x2: in_line = '\033[92m>\033[0m'
                    elif x + MIN_X in range(l.x1 + 1, l.x2): in_line = '\033[91m-\033[0m'
                    else: continue
                    if before is not None: crash = True
                    break
            print(in_line if in_line is not None else char, end='')
        print()
    print(i)
    if i == SEARCH:
        pass
    if crash:
        print(lines)
        exit(-1)
    input()
    i += 1