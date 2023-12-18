import os
import part2debugger as db

USE_FAKE = True
DEBUG = USE_FAKE and True

class Line:
    def __init__(self, x1, x2, y):
        self.x1 = min(x1, x2)
        self.x2 = max(x1, x2)
        self.y = y
    
    def __repr__(self) -> str:
        return f"<{self.x1}, {self.x2}, {self.y}>"
    
    def __str__(self) -> str:
        return self.__repr__()
    
    def __len__(self):
        return self.x2 - self.x1 + 1

lines = []
x, y = 0, 0
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    for l in f:
        fr, fd, hex = l.split()
        r = hex[-2]
        d = int(hex[2:-2], base=16)
        if USE_FAKE:
            r = {'R': '0', 'D': '1', 'L': '2', 'U': '3'}[fr]
            d = int(fd)
        if r == '0':
            lines.append(Line(x, x + d, y))
            x += d
        elif r == '2':
            lines.append(Line(x, x - d, y))
            x -= d
        elif r == '1':
            y += d
        elif r == '3':
            y -= d

lines.sort(key=lambda x: x.x1)
lines.sort(key=lambda x: x.y)

line_groups = []
consec = 1
for i, l in enumerate(lines[:-1]):
    if l.y == lines[i+1].y: consec += 1
    else:
        line_groups.append(lines[i-consec+1:i+1])
        consec = 1
line_groups.append(lines[-consec:])


cur = line_groups[0]
if DEBUG: db.PRINT_MAP(cur)
area = sum(map(len, line_groups[0]))
for i, add in enumerate(line_groups[1:]):
    y = add[0].y
    if DEBUG: db.PRINT_MAP(cur + [Line(x.x1, x.x2, y) for x in cur])
    area += sum(map(len, cur)) * (y - cur[0].y - 1)
    cur = [Line(x.x1, x.x2, y) for x in cur]
    j, k = 0, 0
    while j < len(cur) and len(add) - k > 0:
        if add[k].x2 == cur[j].x1:
            cur[j].x1 = add.pop(k).x1
        elif cur[j].x2 == add[k].x1:
            if j + 1 < len(cur) and add[k].x2 == cur[j+1].x1:
                cur[j].x2 = cur.pop(j+1).x2
                add.pop(k)
            else:
                cur[j].x2 = add.pop(k).x2
                j += 1
        elif add[k].x2 < cur[j].x1:
            cur.insert(j, add.pop(k))
            j += 1
        elif cur[j].x1 <= add[k].x1 or add[k].x2 <= cur[j].x2:
            k += 1
        else:
            j += 1
    if DEBUG: db.PRINT_MAP(cur)
    area += sum(map(len, cur))
    j = 0
    while j < len(cur) and len(add) > 0:
        if cur[j].x1 == add[0].x1:
            if add[0].x2 == cur[j].x2:
                cur.pop(j)
                add.pop(0)
            else:
                cur[j].x1 = add.pop(0).x2
        elif add[0].x2 == cur[j].x2:
            cur[j].x2 = add.pop(0).x1
            j += 1
        elif cur[j].x1 < add[0].x1 and add[0].x2 < cur[j].x2:
            cur.insert(j, Line(cur[j].x1, add[0].x1, y))
            j += 1
            cur[j].x1 = add[0].x2
            add.pop(0)
        else:
            j += 1

print(area)
