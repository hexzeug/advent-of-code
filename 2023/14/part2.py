import os
from sys import getsizeof
import numpy as np

platform = None
with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    platform = np.array([[['.', 'O', '#'].index(y) for y in x.strip()] for x in f], dtype=np.byte)

def roll(col):
    f = 0
    col = list(col)
    for i, x in enumerate(col):
        if x == 2:
            f = i + 1
        elif x == 1:
            col[f], col[i] = col[i], col[f]
            f += 1
    return np.array(col)

platform.flags.writeable = False

TIMES = 1_000_000_000
# TIMES = 21

cache = dict()
path = []
i = None

for i in range(TIMES):
    print(f"{i // (TIMES / 100000) / 1000}%", getsizeof(cache), len(cache))
    before = platform.data.tobytes()
    if before in cache:
        break
    for j in range(4):
        platform = np.rot90(np.apply_along_axis(roll, 0, platform), 3)
    path.append(before)
    cache[before] = platform

print(platform)

s = path.index(platform.data.tobytes())
cycle_length = i - s
offset_in_cycle = (TIMES - i) % cycle_length
print(s, cycle_length, offset_in_cycle, s + offset_in_cycle)


platform = np.frombuffer(path[s + offset_in_cycle], dtype=np.byte).reshape(platform.shape)

print(platform)

res = sum([np.count_nonzero(x == 1) * (i + 1) for i, x in enumerate(reversed(platform))])
          
print(res)