import os
from itertools import groupby

sum = 0
pos = 0
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    for line in f:
        if pos % 100 == 0: print(pos)
        row, groups = line.rstrip().split()
        row = list(row)
        groups = list(map(int, groups.split(',')))
        unknown = [i for i, x in enumerate(row) if x == '?']
        for i in range(2**len(unknown)):
            for j, x in enumerate(unknown):
                row[x] = ['.', '#'][(i >> j) & 1]
            grouped_row = [len(list(y)) for x, y in groupby(row) if x == '#']
            sum += grouped_row == groups
        pos += 1

print(sum)

            