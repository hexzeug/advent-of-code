import os

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
            instr.append((d, 0, 0, 1))
        elif r == 'L':
            lr -= d
            min_l = min(min_l, lr)
            instr.append((-d, 0, 0, -1))
        elif r == 'D':
            tb += d
            max_b = max(max_b, tb)
            instr.append((0, d, -1, 0))
        elif r == 'U':
            tb -= d
            min_t = min(min_t, tb)
            instr.append((0, -d, 1, 0))
        
mp = [[0] * (max_r - min_l + 1) for _ in range(max_b - min_t + 1)]

right = set()

x, y = -min_l, -min_t
for dx, dy, rx, ry in instr:
    for i in range(min(y, y + dy), max(y, y + dy) + 1):
        for j in range(min(x, x + dx), max(x, x + dx) + 1):
            if mp[i][j] == 1: right.remove((i, j))
            mp[i][j] = 2
            ri = i + ry
            rj = j + rx
            if mp[ri][rj] == 0:
                mp[ri][rj] = 1
                right.add((ri, rj))
    x += dx
    y += dy

right_is_outside = False
while len(right) > 0:
    i, j = right.pop()
    for ni, nj in [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]:
        if ni not in range(len(mp)) or nj not in range(len(mp[0])):
            right_is_outside = True
            continue
        if mp[ni][nj] == 0:
            mp[ni][nj] = 1
            right.add((ni, nj))

for l in mp: print(''.join(map(str, l)))
print('count', '0' if right_is_outside else '1', '+ 2\n')

print(len([None for l in mp for c in l if c != (1 if right_is_outside else 0)]))