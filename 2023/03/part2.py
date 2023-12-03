import os
from functools import reduce

schematic = None
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    schematic = [l[:-1] for l in f]

sum = 0
id = 0
for i, line in enumerate(schematic):
    j = 0
    row = [False] * len(line)
    while j < len(line):
        if (line[j].isdigit()):
            s = j
            while j < len(line) and line[j].isdigit(): j += 1
            number = int(line[s:j])
            for x in range(s, j): row[x] = id, number
            id += 1
        else:
            if line[j] == '*': row[j] = True
            j += 1
    schematic[i] = row
for i, row in enumerate(schematic):
    for j, type in enumerate(row):
        if type == True:
            s_x = max(j - 1, 0)
            s_y = max(i - 1, 0)
            e_x = min(j + 2, len(row))
            e_y = min(i + 2, len(schematic))
            parts = {x for l in schematic[s_y:e_y] for x in l[s_x:e_x] if isinstance(x, tuple)}
            if len(parts) == 2: sum += reduce(lambda a, b: a[1] * b[1], parts)
print(sum)
