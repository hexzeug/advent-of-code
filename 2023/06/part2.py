import os

with open(f"{os.path.dirname(__file__)}/input.txt") as f:
    t, r = map(lambda x: int(x.rstrip().split(': ')[1].replace(' ', '')), f.readlines()[:2])
    x = t // 2
    y = t // 2 + (t & 0x1)
    d = 0
    while (x - d) * (y + d) > r:
        d += 1
    print(d * 2 - 1 + (t & 0x1))
