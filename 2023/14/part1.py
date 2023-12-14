import os

fixed_stones = []
load = 0
load_count = 0
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    for i, line in enumerate(f):
        line = line.strip()
        if len(fixed_stones) == 0: fixed_stones = [-1] * len(line)
        load += load_count
        for j, stone in enumerate(line):
            if stone == 'O':
                load += i - fixed_stones[j]
                load_count += 1
                fixed_stones[j] += 1
            elif stone == '#':
                fixed_stones[j] = i

print(load)
